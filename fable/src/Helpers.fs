namespace System

type 'a ReadOnlySpan (arr:'a array, start : int, len : int) =
    member x.Len = len
    member x.IsEmpty = false

type 'a ReadOnlyMemory = 'a ReadOnlySpan


namespace System.Buffers.Binary
module BinaryPrimitives =
    let ReadUInt64BigEndian v = 1UL
    let ReadInt64BigEndian v = 1L
    let ReadUInt32BigEndian v = 1u
    let ReadInt32BigEndian v = 1
    let ReadUInt16BigEndian v = 1us
    let ReadInt16BigEndian v = 1s
