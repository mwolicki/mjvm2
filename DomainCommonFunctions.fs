module DomainCommonFunctions

open Domain

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
