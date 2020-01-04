module DomainEnricher
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

    {
        AccessFlags = classFile.AccessFlags
        ThisClass = getClassInfo classFile.ThisClass
        SuperClass = classFile.SuperClass |> Option.map getClassInfo
        Interfaces = classFile.Interfaces |> List.map getClassInfo
    }
