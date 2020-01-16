#if INTERACTIVE
#load "Domain.fs"
#else
module Parser
#endif
open Domain
open Domain.Lower
open System
open System.IO
open System.Buffers.Binary
open System.Collections.Generic

[<Struct>]
[<StructuredFormatDisplay("State = {AsArray}; Pos = {Pos}")>]
type State = { Data : ReadOnlyMemory<byte>; Pos : int }
with 
    member s.Span = s.Data.Span
    member s.ReadOnlyMemorySlice len = s.Data.Slice (0, len)
    member s.AsArray = s.Span.ToArray ()
    member s.Slice len = s.Span.Slice (0, len)
    member s.Item with get i = s.Span.[i]
    static member inline (++) (x:State, i) = { x with Data = x.Data.Slice i; Pos = x.Pos + i }
    static member init data = { Data = data; Pos = 0 }

type OptionBuilder () =
    //M<'T> * ('T -> M<'U>) -> M<'U>
    member inline _.Bind (m, f) = m |> Option.bind f
    member _.Zero () = None
    member _.Yield v = Some v
    member _.YieldFrom v = v
    member _.Return v = Some v
    member _.ReturnFrom (v: _ option) = v
    member _.Delay f = f 
    member _.Run f = f ()
    member _.Combine (a, b) = 
        match a,b with
        | Some a, Some b -> Some (a, b)
        | _ -> None


let option = OptionBuilder ()

[<Struct>]
type Result<'a> = { State : State; Result : 'a }

type Parse<'b> = State -> Result<'b> option

let inline (=~) (a:Parse<'a>) (f:'a->'b) = fun x -> a x |> Option.map (fun a -> { State = a.State; Result = f a.Result })
let inline (=>.) (a:Parse<'a>) (b:Parse<'b>) : Parse<_> = fun x -> a x |> Option.map (fun a -> b a.State) |> Option.flatten
let inline (.=>) (a:Parse<'a>) (b:Parse<'b>) : Parse<'a> = 
    fun x -> 
        option {
            let! a = a x
            let! b = b a.State
            return { State = b.State; Result = a.Result }
        }

let inline (.=>.) (a:Parse<'a>) (b:Parse<'b>) : Parse<('a*'b)> = 
    fun x -> 
        option {
            let! a = a x
            let! b = b a.State
            return { State = b.State; Result = a.Result, b.Result }
        }


let inline res state result = Some { State = state; Result = result }

let inline pSingleton v state = res state v

let pFloat (ms:State) = res (ms ++ 8) (BitConverter.ToDouble (ms.Slice 8))
let pFloat32 (ms:State) = res (ms ++ 4) (BitConverter.ToSingle (ms.Slice 4))
let u8 (ms:State) = res (ms ++ 8) (BinaryPrimitives.ReadUInt64BigEndian (ms.Slice 8))
let u4 (ms:State) = res (ms ++ 4) (BinaryPrimitives.ReadUInt32BigEndian (ms.Slice 4))
let u2 (ms:State) = res (ms ++ 2) (BinaryPrimitives.ReadUInt16BigEndian (ms.Slice 2))
let u1 (ms:State) = res (ms ++ 1) ms.[0]

let i8 (ms:State) = res (ms ++ 8) (BinaryPrimitives.ReadInt64BigEndian (ms.Slice 8))
let i4 (ms:State) = res (ms ++ 4) (BinaryPrimitives.ReadInt32BigEndian (ms.Slice 4))
let i2 (ms:State) = res (ms ++ 2) (BinaryPrimitives.ReadInt16BigEndian (ms.Slice 2))

let choice (parsers:Parse<_> list) = 
    fun x ->
        let rec loop = function
        | [] -> None
        | parser::xs -> 
            match parser x with
            | Some _ as success -> success
            | None -> loop xs
        loop parsers

let indexedChoice (indexParser:'a Parse) (parsers:('a * Parse<_>) list) = 
    let parsersMap = parsers |> Map.ofList |> Dictionary

    fun state ->
        option {
            let! index = indexParser state
            return! 
                match parsersMap.TryGetValue index.Result with
                | true, parser -> parser index.State
                | false, _ -> None
        }

let inline isVal (parser : Parse<'a>) (value : 'a) =
    fun x -> parser x |> Option.bind(fun x-> if x.Result = value then Some x else None)


let readFile path = File.ReadAllBytes path |> ReadOnlyMemory |> State.init


let inline isU1Val v = isVal u1 v


let pRefKind =
    indexedChoice u1 [ 1uy, pSingleton GetField; 2uy, pSingleton GetStatic; 
                       3uy, pSingleton PutField; 4uy, pSingleton PutStatic; 
                       5uy, pSingleton InvokeVirtual; 6uy, pSingleton InvokeStatic; 
                       7uy, pSingleton InvokeSpecial; 8uy, pSingleton NewInvokeSpecial; 
                       9uy, pSingleton InvokeInterface; ]

let pRefInfo = u2 .=>. u2 =~ (fun (classIndex, nameAndTypeIndex) -> {ClassIndex = classIndex; NameAndTypeIndex = nameAndTypeIndex} )

let pCUtf8 = 
    let pUtf8 : Parse<String> = fun  state ->
        option {
            let! sizeResult = u2 state 
            let size = sizeResult.Result |> int
            return! res (sizeResult.State ++ size) (System.Text.Encoding.UTF8.GetString (sizeResult.State.Slice size))
        }
    1uy , pUtf8 =~ CUtf8
let pCInt = 3uy, i4 =~ CInteger
let pCFloat = 4uy, pFloat32 =~ CFloat
let pCLong = 5uy, i8 =~ CLong
let pCDouble = 6uy, pFloat =~ CDouble
let pCClass = 7uy, u2 =~ (Utf8Index >> CClass)
let pCString = 8uy, u2 =~ (Utf8Index >> CString)
let pCFieldref = 9uy, pRefInfo =~ CFieldref
let pCMethodType = 10uy, pRefInfo =~ CMethodref
let pCInterfaceMethodref = 11uy, pRefInfo =~ CInterfaceMethodref
let pCNameAndType = 12uy, u2 .=>. u2 =~ (fun (classIndex, nameAndTypeIndex) -> {NameIndex = classIndex; DescriptorIndex = Utf8Index nameAndTypeIndex} |> CNameAndType)

let pCMethodHandle = 15uy, pRefKind .=>. u2 =~ (fun (refKind, refIndex) -> {ReferenceKind = refKind; ReferenceIndex = refIndex } |> CMethodHandle)

let pCDynamic = 17uy, u2 .=>. u2 =~ (fun (bootstrapMethodAttrIndex, nameAndTypeIndex) -> {BootstrapMethodAttrIndex = bootstrapMethodAttrIndex; NameAndTypeIndex = nameAndTypeIndex } |> CDynamic)
let pCInvokeDynamic = 18uy, u2 .=>. u2 =~ (fun (bootstrapMethodAttrIndex, nameAndTypeIndex) -> {BootstrapMethodAttrIndex = bootstrapMethodAttrIndex; NameAndTypeIndex = nameAndTypeIndex } |> CInvokeDynamic)

let pCModule = 19uy, u2 =~ (Utf8Index >> CModule)
let pCPackage = 20uy, u2 =~ (Utf8Index >> CPackage)

let pConstType : Parse<ConstantType> = indexedChoice u1 [pCUtf8; pCInt; pCFloat; 
    pCLong; pCDouble; pCClass; pCString; pCFieldref; pCMethodType; 
    pCInterfaceMethodref; pCNameAndType; pCMethodHandle;
    pCDynamic; pCInvokeDynamic; pCModule; pCPackage]

let parseConsts = 
    let rec loop state count index xs : Result<IReadOnlyDictionary<_, _>> option =
        if count <= 0us then 
            res state (xs |> Map.ofList |> Dictionary :> _) 
        else 
            option {
                let! p = pConstType state
                let count, nextIndex = 
                    match p.Result with
                    | CDouble _ | CLong _ -> count - 2us, index + 2us
                    | _ -> count - 1us, index + 1us
                let xs = (index, p.Result) :: xs
                return! loop p.State count nextIndex xs }

    fun x -> 
        option {
            let! constCount = u2 x
            return! loop constCount.State (constCount.Result - 1us) 1us [] }

let pAccessFlags = u2 =~ (int32 >> enum<AccessFlag>)
let pVersion = u2 .=>. u2 =~ fun (minor, major) -> { MinorVersion = minor; MajorVersion = major }

let pThisClass = u2 =~ ClassInfo
let pSuperClass = u2 =~ function | 0us -> None | ci -> ClassInfo ci |> Some

let inline parseLoop f = 
    let rec loop state count xs : Result<_ list> option =
        if count <= 0us then 
            res state (xs |> List.rev) 
        else 
            match f state with
            | Some { Result = result; State = state } ->
                loop state (count - 1us) (result :: xs)
            | None -> None

    fun state -> 
        option {
            let! { Result = count; State = state } = u2 state
            return! loop state count [] }

let parseLoopU4 f = 
    let rec loop state destPosition xs : Result<_ list> option =
        if destPosition = state.Pos then 
            res state (xs |> List.rev) 
        elif destPosition < state.Pos then 
            failwithf "Unexpected position - expected %d got %d" destPosition state.Pos
        else 
            match f state with
            | Some { Result = result; State = state } ->
                loop state destPosition (result :: xs)
            | None -> None

    fun state -> option {
            let! { Result = count; State = state } = u4 state
            let! state' = loop { Data = state.Data.Slice (0, int count); Pos = 0 } (int count) []
            return { state' with State = state ++ (int count) } }

let pInterfaces = parseLoop (u2 =~ ClassInfo)

let pAttributeInfo = 
    let parseAttributeBytes state =
        option {
            let! { Result = size; State = state } = u4 state
            let size = int size

            return! res (state ++ size) (state.ReadOnlyMemorySlice size) } 
    u2 .=>. parseAttributeBytes =~ fun (index, bytes) -> { AttributeNameIndex = Utf8Index index; Info = bytes }

let pFields = parseLoop (pAccessFlags .=>. u2 .=>. u2 .=>. (parseLoop pAttributeInfo) =~
                    fun (((accessFlags, nameIndex), descriptorIndex), attricuteInfo) ->
                    { FieldInfo.AccessFlags = accessFlags
                      NameIndex = Utf8Index nameIndex
                      DescriptorIndex = Utf8Index descriptorIndex
                      AttributeInfo = attricuteInfo })

let pMethods = parseLoop (pAccessFlags .=>. u2 .=>. u2 .=>. (parseLoop pAttributeInfo) =~
                    fun (((accessFlags, nameIndex), descriptorIndex), attricuteInfo) ->
                    { MethodInfo.AccessFlags = accessFlags
                      NameIndex = Utf8Index nameIndex
                      DescriptorIndex = Utf8Index descriptorIndex
                      AttributeInfo = attricuteInfo })

let pAttributes = parseLoop pAttributeInfo

let eom (state:State) = 
    if state.Span.IsEmpty then res state () else None

let parseHeader = 
    (u4 .=>. pVersion .=>. parseConsts .=>. pAccessFlags .=>. pThisClass .=>. pSuperClass .=>. pInterfaces .=>. pFields .=>. pMethods .=>. pAttributes .=> eom) 
    =~ fun (((((((((magicNumber, version), consts), accessFlags), thisClass) , superClass), interfaces), fields), methods), attributes) -> 
        { Magic = magic magicNumber;
          Version = version
          ConstantPool = consts
          AccessFlags = accessFlags
          ThisClass = thisClass
          SuperClass = superClass
          Interfaces = interfaces
          Fields = fields
          Methods = methods
          Attributes = attributes }

open Higher

let indexedChoice' (indexParser:uint8 Parse) (parsers:(uint8 * Parse<_>) list) = 
    let parsersMap = parsers |> Map.ofList |> Dictionary
    fun state ->
        option {
            let! index = indexParser state
            return! match parsersMap.TryGetValue index.Result with
                    | true, parser -> parser index.State
                    | false, _ -> 
                        //failwithf "Unknown ops 0x%x (%d)" index.Result index.Result 
                        eprintfn "Unknown ops 0x%x (%d)" index.Result index.Result 
                        res { index.State with Pos = index.State.Pos + index.State.Data.Length } ( state.Data.ToArray () |> OpsCode.Unknown) }
let getArrayType = function
| 4uy -> ArrayType.T_BOOLEAN
| 5uy -> ArrayType.T_CHAR
| 6uy -> ArrayType.T_FLOAT
| 7uy -> ArrayType.T_DOUBLE
| 8uy -> ArrayType.T_BYTE
| 9uy -> ArrayType.T_SHORT
| 10uy -> ArrayType.T_INT
| 11uy -> ArrayType.T_LONG
| x -> failwithf "Unknown array type %d" x

let pOpsCode = indexedChoice' u1 [
        0uy, pSingleton OpsCode.Nop
        1uy, pSingleton OpsCode.Aconst_null
        2uy, pSingleton (OpsCode.IConst -1y)
        3uy, pSingleton (OpsCode.IConst 0y)
        4uy, pSingleton (OpsCode.IConst 1y)
        5uy, pSingleton (OpsCode.IConst 2y)
        6uy, pSingleton (OpsCode.IConst 3y)
        7uy, pSingleton (OpsCode.IConst 4y)
        8uy, pSingleton (OpsCode.IConst 5y)
        9uy, pSingleton (OpsCode.LConst 0uy)
        10uy, pSingleton (OpsCode.LConst 1uy)
        11uy, pSingleton (OpsCode.FConst 0.f)
        12uy, pSingleton (OpsCode.FConst 1.f)
        13uy, pSingleton (OpsCode.FConst 2.f)
        14uy, pSingleton (OpsCode.DConst 0.)
        15uy, pSingleton (OpsCode.DConst 1.)
        16uy, u1 =~ OpsCode.BiPush
        17uy, i2 =~ OpsCode.SiPush
        18uy, u1 =~ OpsCode.Ldc
        20uy, u2 =~ OpsCode.Ldc2_w
        21uy, u1 =~ OpsCode.Iload
        25uy, u1 =~ OpsCode.Aload
        26uy, pSingleton (OpsCode.Iload 0uy)
        27uy, pSingleton (OpsCode.Iload 1uy)
        28uy, pSingleton (OpsCode.Iload 2uy)
        29uy, pSingleton (OpsCode.Iload 3uy)
        42uy, pSingleton (OpsCode.Aload 0uy)
        43uy, pSingleton (OpsCode.Aload 1uy)
        44uy, pSingleton (OpsCode.Aload 2uy)
        45uy, pSingleton (OpsCode.Aload 3uy)
        46uy, pSingleton OpsCode.LaStore
        50uy, pSingleton OpsCode.AaLoad
        54uy, u1 =~ OpsCode.IStore
        56uy, u1 =~ OpsCode.FStore
        57uy, u1 =~ OpsCode.DStore
        58uy, u1 =~ OpsCode.AStore
        59uy, pSingleton (OpsCode.IStore 0uy)
        60uy, pSingleton (OpsCode.IStore 1uy)
        61uy, pSingleton (OpsCode.IStore 2uy)
        62uy, pSingleton (OpsCode.IStore 3uy)
        67uy, pSingleton (OpsCode.FStore 0uy)
        68uy, pSingleton (OpsCode.FStore 1uy)
        69uy, pSingleton (OpsCode.FStore 2uy)
        70uy, pSingleton (OpsCode.FStore 3uy)
        72uy, pSingleton (OpsCode.DStore 0uy)
        73uy, pSingleton (OpsCode.DStore 1uy)
        74uy, pSingleton (OpsCode.DStore 2uy)
        75uy, pSingleton (OpsCode.AStore 3uy)
        76uy, pSingleton (OpsCode.AStore 1uy)
        77uy, pSingleton (OpsCode.AStore 2uy)
        78uy, pSingleton (OpsCode.AStore 3uy)
        80uy, pSingleton OpsCode.LaStore
        86uy, pSingleton OpsCode.SaStore
        89uy, pSingleton OpsCode.Dup
        96uy, pSingleton OpsCode.Swap
        176uy, pSingleton OpsCode.Areturn
        177uy, pSingleton OpsCode.ReturnVoid
        178uy, u2 =~ OpsCode.GetStatic
        181uy, u2 =~ OpsCode.PutField
        182uy, u2 =~ OpsCode.InvokeVirtual
        183uy, u2 =~ OpsCode.InvokeSpecial
        184uy, u2 =~ OpsCode.InvokeStatic
        188uy, u1 =~ (getArrayType >> OpsCode.NewArray)
        189uy, u2 =~ OpsCode.AnewArray
        197uy, u2 .=>. u1 =~ OpsCode.MultiAnewArray
        198uy, u2 =~ OpsCode.IfNull
        199uy, u2 =~ OpsCode.IfNonNull
    ]

let pExceptionEntry =
    u2 .=>. u2 .=>. u2 .=>. u2 =~ fun (((startPc, endPc), handlerPc), catchType) -> {
        StartPc = startPc
        EndPc = endPc
        HandlerPc = handlerPc
        CatchType = catchType
    }

let pLineNumberAttr = u2 .=>. u2 =~ fun (startPc, lineNum) -> { LineNumber = lineNum; StartPc = startPc }

let pLineNumber = 
    let p = parseLoop pLineNumberAttr
    fun data ->
        State.init data |> p |> function | Some { Result = result } -> result | None -> failwith "Failed to parse line number table attribute."

let parseCode getAttribute = 
    
    let pCode = parseLoopU4 pOpsCode
    let p = (u2 .=>. u2 .=>. pCode .=>. (parseLoop pExceptionEntry) .=>. pAttributes =~ fun ((((maxStack, maxLocals), code), exceptionEntry), attributes) -> {
        Higher.CodeAttribute.MaxStack = maxStack
        Higher.CodeAttribute.MaxLocals = maxLocals
        Higher.CodeAttribute.Code = code
        Higher.CodeAttribute.ExceptionTable = exceptionEntry
        Higher.CodeAttribute.Attributes = getAttribute attributes
    })
    fun data ->
    State.init data |> p |> function | Some { Result = result } -> result | None -> failwith "Failed to parse code attribute."
