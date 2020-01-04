module DomainEnricher
open System
open Domain
open Domain.Higher

let transform (classFile : Lower.ClassFile) : ClassFile =

    let getUtf8 (Lower.Utf8Index index) = 
        match classFile.ConstantPool.TryGetValue index with
        | true, Lower.CUtf8 str -> str
        | true, x -> failwithf "Wrong const type. Expected CUtf8, got: %A" x
        | false, _ -> failwithf "Unknown index: %d" index
    let getClassInfo (Lower.ClassInfo index) =
        match classFile.ConstantPool.TryGetValue index with
        | true, Lower.CClass classInfo -> getUtf8 classInfo
        | true, x -> failwithf "Wrong const type. Expected CClass, got: %A" x
        | false, _ -> failwithf "Unknown index: %d" index

    let rec getFieldDescriptor (s:string) =
        match s.[0] with
        | 'B' -> FieldDescriptor.Byte
        | 'C' -> FieldDescriptor.Char
        | 'D' -> FieldDescriptor.Double
        | 'F' -> FieldDescriptor.Float
        | 'I' -> FieldDescriptor.Integer
        | 'J' -> FieldDescriptor.Long
        | 'L' when s.EndsWith ";" -> s.Substring (1, s.Length - 2) |> ClassName |> FieldDescriptor.Reference 
        | 'S' -> FieldDescriptor.Short
        | 'Z' -> FieldDescriptor.Boolean
        | '[' -> getFieldDescriptor (s.Substring 1) |> FieldDescriptor.Array
        | _ -> failwithf "Unknown recognise field descriptor: '%s'" s

    let getFieldInfo (fieldInfo:Lower.FieldInfo) =
        {
            FieldInfo.AccessFlags = fieldInfo.AccessFlags
            Name = getUtf8 fieldInfo.NameIndex
            Descriptor = getUtf8 fieldInfo.DescriptorIndex |> getFieldDescriptor
        }
    {
        AccessFlags = classFile.AccessFlags
        ThisClass = getClassInfo classFile.ThisClass
        SuperClass = classFile.SuperClass |> Option.map getClassInfo
        Interfaces = classFile.Interfaces |> List.map getClassInfo
        Fields = classFile.Fields |> List.map getFieldInfo
    }
