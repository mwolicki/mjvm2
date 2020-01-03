#if INTERACTIVE
#load "Domain.fs"
#else
module Myjvm2
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


[<Struct>]
type Result<'a> = { State : State; Result : 'a }

type Parse<'b> = State -> Result<'b>

let inline (=~) (a:Parse<'a>) (f:'a->'b) = fun x -> let a = a x in { State = a.State; Result = f a.Result}
let inline (=>.) (a:Parse<'a>) (b:Parse<'b>) : Parse<_> = fun x -> b (a x).State
let inline (.=>) (a:Parse<'a>) (b:Parse<'b>) : Parse<'a> = 
    fun x -> 
        let a = a x
        let b = b a.State
        { State = b.State; Result = a.Result }
let inline (.=>.) (a:Parse<'a>) (b:Parse<'b>) : Parse<('a*'b)> = 
    fun x -> 
        let a = a x
        let b = b a.State
        { State = b.State; Result = a.Result, b.Result }


let inline res state result = { State = state; Result = result }

let u8 (ms:State) = res (ms ++ 8) (BinaryPrimitives.ReadUInt64BigEndian (ms.Slice 8))
let u4 (ms:State) = res (ms ++ 4) (BinaryPrimitives.ReadUInt32BigEndian (ms.Slice 4))
let u2 (ms:State) = res (ms ++ 2) (BinaryPrimitives.ReadUInt16BigEndian (ms.Slice 2))
let u1 (ms:State) = res (ms ++ 1) ms.Data.[0]

let i8 (ms:State) = res (ms ++ 8) (BinaryPrimitives.ReadInt64BigEndian (ms.Slice 8))
let i4 (ms:State) = res (ms ++ 4) (BinaryPrimitives.ReadInt32BigEndian (ms.Slice 4))
let i2 (ms:State) = res (ms ++ 2) (BinaryPrimitives.ReadInt16BigEndian (ms.Slice 2))


let readFile path = { Data = File.ReadAllBytes path; Pos = 0 }

let parseHeader = ((u4 .=>. u2) .=>. u2) =~ fun ((magicNumber, minor), major) -> { Magic = magic magicNumber; MinorVersion = minor; MajorVersion = major }

let x =  readFile "/Users/kevin/_projects/java/HelloWorld.class" |> parseHeader
