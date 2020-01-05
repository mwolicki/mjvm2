module DomainEnricher
open System
open Domain
open Domain.Higher
open System.Buffers.Binary
open DomainCommonFunctions

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

let getAttribute (classFile : Lower.ClassFile) (ai:Lower.AttributeInfo) =
    let getAttributeConst (info:ReadOnlyMemory<byte>) = 
        let index = BinaryPrimitives.ReadUInt16BigEndian info.Span
        match classFile.ConstantPool.TryGetValue index with
        | true, Lower.CString utf8 -> getUtf8 classFile utf8 |> AttributeConst.String
        | true, Lower.CDouble value -> AttributeConst.Double value
        | true, Lower.CFloat value -> AttributeConst.Float value
        | true, Lower.CInteger value -> AttributeConst.Integer value
        | true, Lower.CLong value -> AttributeConst.Long value
        | true, unknown -> failwithf "Unknown const: %A for attribute: %A" unknown ai
        | false, _ -> failwithf "Unknown index: %d for attribute: %A" index ai

    let name = getUtf8 classFile ai.AttributeNameIndex
    match name with
    | "ConstantValue" -> getAttributeConst ai.Info |> Const
    | "SourceFile" -> 
        BinaryPrimitives.ReadUInt16BigEndian ai.Info.Span
        |> Lower.Utf8Index |> getUtf8 classFile |> SourceFile
    | "Code" ->
        Parser.parseCode ai.Info |> Code
    | _ -> Unsupported {| Name = name; Info = ai.Info |}


let transform (classFile : Lower.ClassFile) : ClassFile =

    let getFieldInfo (fieldInfo:Lower.FieldInfo) =
        {
            FieldInfo.AccessFlags = fieldInfo.AccessFlags
            Name = getUtf8 classFile fieldInfo.NameIndex
            Descriptor = getUtf8 classFile fieldInfo.DescriptorIndex |> fun s -> let span = s.AsSpan () in getFieldDescriptor (span)
            Attributes = fieldInfo.AttributeInfo |> List.map (getAttribute classFile)
        }


    let getMethodInfo (methodInfo:Lower.MethodInfo) =
        {
            MethodInfo.AccessFlags = methodInfo.AccessFlags
            Name = getUtf8 classFile methodInfo.NameIndex
            Descriptor = getUtf8 classFile methodInfo.DescriptorIndex
            Attributes = methodInfo.AttributeInfo |> List.map (getAttribute classFile)
        }
    {
        AccessFlags = classFile.AccessFlags
        ThisClass = getClassInfo classFile classFile.ThisClass
        SuperClass = classFile.SuperClass |> Option.map (getClassInfo classFile)
        Interfaces = classFile.Interfaces |> List.map (getClassInfo classFile)
        Fields = classFile.Fields |> List.map getFieldInfo
        Methods = classFile.Methods |> List.map getMethodInfo
        Attributes = classFile.Attributes |> List.map (getAttribute classFile)
    }
