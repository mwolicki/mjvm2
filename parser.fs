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
type State = { Data : byte array; Pos : int }
with 
    member s.Span = ReadOnlySpan(s.Data, s.Pos, s.Data.Length - s.Pos)
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

let pRefInfo = u2 .=>. u2 =~ (fun (classIndex, nameAndTypeIndex) -> {ClassIndex = classIndex; NameAndTypeIndex = nameAndTypeIndex} )

let pCInt = isU1Val 3uy =>. i4 =~ CInteger
let pCFloat = isU1Val 4uy =>. pFloat32 =~ CFloat
let pCLong = isU1Val 5uy =>. i8 =~ CLong
let pCDouble = isU1Val 6uy =>. pFloat =~ CDouble
let pCFieldref = isU1Val 9uy =>. pRefInfo =~ CFieldref
let pCMethodType = isU1Val 10uy =>. pRefInfo =~ CMethodref
let pCInterfaceMethodref = isU1Val 11uy =>. pRefInfo =~ CInterfaceMethodref

//we could make this a lot faster by not cheking always all possible combinations
let pConstType : Parse<ConstantType> = choice [pCInt; pCFloat; pCLong; pCDouble; pCFieldref; pCMethodType; pCInterfaceMethodref]
(*
let toConstType = function
| 1uy -> CUtf8
| 3uy -> CInteger
| 4uy -> CFloat
| 5uy -> CLong
| 6uy -> CDouble
| 7uy -> CClass
| 8uy -> CString
| 9uy -> CFieldref
| 10uy -> CMethodref
| 11uy -> CInterfaceMethodref
| 12uy -> CNameAndType
| 15uy -> CMethodHandle
| 16uy -> CMethodType
| 17uy -> CDynamic
| 18uy -> CInvokeDynamic
| 19uy -> CModule
| 20uy -> CPackage
| unknownConstType -> failwithf "Unknown const type - number %d" unknownConstType
*)

let parseConsts = 
    let rec loop state count xs : Result<_ list> option =
        if count = 0us then 
            res state (xs |> List.rev)
        else 
            option {
                let! p = pConstType state
                printfn "%A" p.Result 
                return! loop p.State (count - 1us) (p.Result :: xs) } |> Option.defaultWith(fun () -> 
                    (u1 =~ (Unknown >> List.singleton)) state |> Option.get) |> Some

    fun x -> 
        option {
            let! constCount = u2 x
            return! loop constCount.State constCount.Result [] }


let x =  readFile "/Users/kevin/_projects/java/HelloWorld.class" |> (parseHeader =>. parseConsts)
printfn "result = %A" x 
