#if INTERACTIVE
#load "Domain.fs"
#else
module Parser
#endif
open Domain
open System
open System.IO
open System.Buffers.Binary

[<Struct>]
[<StructuredFormatDisplay("State = {ReadOnlyMemory}; Pos = {Pos} ")>]
type State = { Data : byte array; Pos : int }
with 
    member s.Span = ReadOnlySpan(s.Data, s.Pos, s.Data.Length - s.Pos)
    member s.ReadOnlyMemory = ReadOnlyMemory(s.Data, s.Pos, s.Data.Length - s.Pos).ToArray()
    member s.Slice len = ReadOnlySpan(s.Data, s.Pos, len)
    static member inline(++) (x:State, i) = { x with Pos = x.Pos + i}

type OptionBuilder () =
    //M<'T> * ('T -> M<'U>) -> M<'U>
    member this.Bind (m, f) = m |> Option.bind f
    member this.Yield v = Some v
    member this.Return v = Some v
    member this.ReturnFrom v = v
    member this.Delay f = f ()
    member this.Combine (a, b) = 
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
let u1 (ms:State) = res (ms ++ 1) ms.Data.[ms.Pos]

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

let inline isVal (parser : Parse<'a>) (value : 'a) =
    fun x -> parser x |> Option.bind(fun x-> if x.Result = value then Some x else None)


let readFile path = { Data = File.ReadAllBytes path; Pos = 0 }

let parseHeader = ((u4 .=>. u2) .=>. u2) =~ fun ((magicNumber, minor), major) -> { Magic = magic magicNumber; MinorVersion = minor; MajorVersion = major }

let inline isU1Val v = isVal u1 v


let pCUtf8 = 
    let pUtf8 : Parse<String> = fun  state ->
        option {
            let! sizeResult = u2 state 
            let size = sizeResult.Result |> int
            return! res (sizeResult.State ++ size) (System.Text.Encoding.UTF8.GetString (sizeResult.State.Slice size))
        }
    isU1Val 1uy =>. pUtf8 =~ CUtf8
let pCInt = isU1Val 3uy =>. i4 =~ CInteger
let pCFloat = isU1Val 4uy =>. pFloat32 =~ CFloat
let pCLong = isU1Val 5uy =>. i8 =~ CLong
let pCDouble = isU1Val 6uy =>. pFloat =~ CDouble
let pCClass = isU1Val 7uy =>. u2 =~ (Utf8Index >> CClass)
let pCString = isU1Val 8uy =>. u2 =~ (Utf8Index >> CString)
let pRefInfo = u2 .=>. u2 =~ (fun (classIndex, nameAndTypeIndex) -> {ClassIndex = classIndex; NameAndTypeIndex = nameAndTypeIndex} )
let pCFieldref = isU1Val 9uy =>. pRefInfo =~ CFieldref
let pCMethodType = isU1Val 10uy =>. pRefInfo =~ CMethodref
let pCInterfaceMethodref = isU1Val 11uy =>. pRefInfo =~ CInterfaceMethodref
let pCNameAndType = isU1Val 12uy =>. u2 .=>. u2 =~ (fun (classIndex, nameAndTypeIndex) -> {NameIndex = classIndex; DescriptorIndex = Utf8Index nameAndTypeIndex} |> CNameAndType)

let pRefKind =
    let inline p v c = isU1Val v =>. pSingleton c
    choice [ p 1uy GetField; p 2uy GetStatic; p 3uy PutField; p 4uy PutStatic
             p 5uy InvokeVirtual; p 6uy InvokeStatic; p 7uy InvokeSpecial
             p 8uy NewInvokeSpecial; p 9uy InvokeInterface ]

let pCMethodHandle = isU1Val 15uy =>. pRefKind .=>. u2 =~ (fun (refKind, refIndex) -> {ReferenceKind = refKind; ReferenceIndex = refIndex } |> CMethodHandle)

let pCDynamic = isU1Val 17uy =>. u2 .=>. u2 =~ (fun (bootstrapMethodAttrIndex, nameAndTypeIndex) -> {BootstrapMethodAttrIndex = bootstrapMethodAttrIndex; NameAndTypeIndex = nameAndTypeIndex } |> CDynamic)
let pCInvokeDynamic = isU1Val 18uy =>. u2 .=>. u2 =~ (fun (bootstrapMethodAttrIndex, nameAndTypeIndex) -> {BootstrapMethodAttrIndex = bootstrapMethodAttrIndex; NameAndTypeIndex = nameAndTypeIndex } |> CInvokeDynamic)

let pCModule = isU1Val 19uy =>. u2 =~ (Utf8Index >> CModule)
let pCPackage = isU1Val 20uy =>. u2 =~ (Utf8Index >> CPackage)

//we could make this a lot faster by not cheking always all possible combinations
let pConstType : Parse<ConstantType> = choice [pCUtf8; pCInt; pCFloat; 
    pCLong; pCDouble; pCClass; pCString; pCFieldref; pCMethodType; 
    pCInterfaceMethodref; pCNameAndType; pCMethodHandle;
    pCDynamic; pCInvokeDynamic; pCModule; pCPackage; isU1Val 0uy =~ Unknown]


let parseConsts = 
    let rec loop state count xs : Result<_ list> option =
        printfn "pos = %d..." count
        if count = 0us then 
            res state (xs |> List.rev)
        else 
            option {
                let! p = pConstType state
                printfn "pos = %d; val = %A" count p.Result 
                let count = 
                    match p.Result with
                    | CDouble _ | CLong _ -> count - 2us
                    | _ -> count - 1us
                return! loop p.State (count) (p.Result :: xs) } |> Option.defaultWith(fun () -> 
                    (u1 =~ (Unknown >> List.singleton)) state |> Option.get) |> Some

    fun x -> 
        option {
            let! constCount = u2 x
            printfn "Count %A" constCount.Result
            return! loop constCount.State constCount.Result [] }


