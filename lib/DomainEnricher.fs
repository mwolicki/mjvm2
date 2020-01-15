module DomainEnricher
open System
open Domain
open Domain.Higher
open System.Buffers.Binary
open DomainCommonFunctions



let rec getDescriptor (s:ReadOnlySpan<_>) =
    match s.[0] with
    | 'B' -> FieldDescriptor.Byte |> Some
    | 'C' -> FieldDescriptor.Char |> Some
    | 'D' -> FieldDescriptor.Double |> Some
    | 'F' -> FieldDescriptor.Float |> Some
    | 'I' -> FieldDescriptor.Integer |> Some
    | 'J' -> FieldDescriptor.Long |> Some
    | 'L' when s.[s.Length - 1] = ';' ->  String (s.Slice (1, s.Length - 2)) |> ClassName |> FieldDescriptor.Reference  |> Some
    | 'S' -> FieldDescriptor.Short |> Some
    | 'Z' -> FieldDescriptor.Boolean |> Some
    | '[' -> 
        let slice  = s.Slice 1
        getDescriptor slice |> Option.map FieldDescriptor.Array
    | _ -> None

let rec private find a pos (x:ReadOnlySpan<_>) =
    if x.Length <= pos then None
    elif Set.contains x.[pos] a then Some pos
    else find a (pos + 1) x


let rec getFieldDescriptor (s:ReadOnlySpan<_>) =
    match getDescriptor s with
    | Some x -> x
    | None -> failwithf "Unrecognised field descriptor '%s'" (String s)

let rec private getMethodDescriptor' acc (s:ReadOnlySpan<_>) =
        match s.[0] with
        | ')' -> 
            let slice  = s.Slice 1
            let returnDesc = getDescriptor slice
            { ParameterDescriptors = List.rev acc; ReturnDescriptor = returnDesc }
        | ',' -> 
            let slice  = s.Slice 1
            getMethodDescriptor' acc slice
        | _ -> 
            let next = 
                match find (Set [')'; ',']) 0 s with
                | Some x -> x
                | None -> failwithf "Unrecognised paramsDescriptor '%s' #0" (String s)
            let paramS = s.Slice (0, next)
            let p = 
                match getDescriptor paramS with
                | Some x -> x 
                | None -> failwithf "Unrecognised paramsDescriptor '%s' #1" (String paramS)

            getMethodDescriptor' (p::acc) (s.Slice next)

let getMethodDescriptor (s:ReadOnlySpan<_>) =
    match s.[0] with
    | '(' -> 
        let slice  = s.Slice 1
        getMethodDescriptor' [] slice
    | _ -> failwithf "Unknown methodDescriptor '%s'" (String s)
    

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
    | "SourceFile" -> 
        BinaryPrimitives.ReadUInt16BigEndian ai.Info.Span
        |> Lower.Utf8Index |> getUtf8 classFile |> SourceFile
    | "Code" ->
        Parser.parseCode (List.map (getAttribute classFile))  ai.Info |> Code
    | "LineNumberTable" -> Parser.pLineNumber ai.Info |> LineNumberTable
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
            Descriptor = getUtf8 classFile methodInfo.DescriptorIndex  |> fun s -> let span = s.AsSpan () in getMethodDescriptor (span)
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
