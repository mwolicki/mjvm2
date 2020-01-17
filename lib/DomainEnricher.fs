module DomainEnricher
open System
open Domain
open Domain.Higher
open System.Buffers.Binary
open DomainCommonFunctions



let rec getAttribute (classFile : Lower.ClassFile) (ai:Lower.AttributeInfo) =
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
    | "Signature" -> 
        BinaryPrimitives.ReadUInt16BigEndian ai.Info.Span
        |> Lower.Utf8Index |> getUtf8 classFile |> Signature
    | "SourceFile" -> 
        BinaryPrimitives.ReadUInt16BigEndian ai.Info.Span
        |> Lower.Utf8Index |> getUtf8 classFile |> SourceFile
    | "Code" ->
        Parser.parseCode (List.map (getAttribute classFile))  ai.Info |> Code
    | "LineNumberTable" -> Parser.pLineNumber ai.Info |> LineNumberTable
    | _ -> Unsupported {| Name = name; Info = ai.Info |}


let inline parseFieldDescriptor data = Parser.parseWrapper Parser.DescriptorParser.pDescriptor "field descriptor" data
let inline parseMethodDescriptor data = Parser.parseWrapper Parser.DescriptorParser.methodDescriptor "method descriptor" data

let transform (classFile : Lower.ClassFile) : ClassFile =
    
    let getFieldInfo (fieldInfo:Lower.FieldInfo) =
        {
            FieldInfo.AccessFlags = fieldInfo.AccessFlags
            Name = getUtf8 classFile fieldInfo.NameIndex
            Descriptor = getUtf8 classFile fieldInfo.DescriptorIndex |> fun x-> x.AsMemory () |> parseFieldDescriptor
            Attributes = fieldInfo.AttributeInfo |> List.map (getAttribute classFile)
        }


    let getMethodInfo (methodInfo:Lower.MethodInfo) =
        {
            MethodInfo.AccessFlags = methodInfo.AccessFlags
            Name = getUtf8 classFile methodInfo.NameIndex
            Descriptor = getUtf8 classFile methodInfo.DescriptorIndex |> fun x-> x.AsMemory () |> parseMethodDescriptor
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
