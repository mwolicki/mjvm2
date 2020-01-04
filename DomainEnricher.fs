module DomainEnricher
open System
open Domain
open Domain.Higher


let getUtf8 (classFile:Lower.ClassFile) (Lower.Utf8Index index) = 
    match classFile.ConstantPool.TryGetValue index with
    | true, Lower.CUtf8 str -> str
    | true, x -> failwithf "Wrong const type. Expected CUtf8, got: %A" x
    | false, _ -> failwithf "Unknown index: %d" index
let getClassInfo (classFile:Lower.ClassFile) (Lower.ClassInfo index) =
    match classFile.ConstantPool.TryGetValue index with
    | true, Lower.CClass classInfo -> getUtf8 classFile classInfo
    | true, x -> failwithf "Wrong const type. Expected CClass, got: %A" x
    | false, _ -> failwithf "Unknown index: %d" index

let rec getFieldDescriptor (s:ReadOnlySpan<_>) =
    match s.[0] with
    | 'B' -> FieldDescriptor.Byte
    | 'C' -> FieldDescriptor.Char
    | 'D' -> FieldDescriptor.Double
    | 'F' -> FieldDescriptor.Float
    | 'I' -> FieldDescriptor.Integer
    | 'J' -> FieldDescriptor.Long
    | 'L' when s.[s.Length - 1] = ';' ->  String (s.Slice (1, s.Length - 2)) |> ClassName |> FieldDescriptor.Reference 
    | 'S' -> FieldDescriptor.Short
    | 'Z' -> FieldDescriptor.Boolean
    | '[' -> 
        let slice  = s.Slice 1
        getFieldDescriptor slice |> FieldDescriptor.Array
    | _ -> failwithf "Unknown recognise field descriptor: '%s'" (String s)


let transform (classFile : Lower.ClassFile) : ClassFile =

    let getFieldInfo (fieldInfo:Lower.FieldInfo) =
        {
            FieldInfo.AccessFlags = fieldInfo.AccessFlags
            Name = getUtf8 classFile fieldInfo.NameIndex
            Descriptor = getUtf8 classFile fieldInfo.DescriptorIndex |> fun s -> let span = s.AsSpan () in getFieldDescriptor (span)
        }
    {
        AccessFlags = classFile.AccessFlags
        ThisClass = getClassInfo classFile classFile.ThisClass
        SuperClass = classFile.SuperClass |> Option.map (getClassInfo classFile)
        Interfaces = classFile.Interfaces |> List.map (getClassInfo classFile)
        Fields = classFile.Fields |> List.map getFieldInfo
    }
