module Domain
open System.Collections.Generic

[<Struct>]
type  Magic = private { Magic:uint32 }
let magic value = if value <> 0xCAFEBABEu then failwithf "Invalid magic number (expected = 0xCAFEBABE, got = %X)!" value else { Magic = value }

[<Struct>]
type RefInfo = { ClassIndex:uint16; NameAndTypeIndex:uint16 }


[<Struct>]
type Utf8Index = Utf8Index of uint16 

[<Struct>]
type NameAndType = { NameIndex : uint16; DescriptorIndex : Utf8Index }

type RefKind = 
| GetField
| GetStatic
| PutField
| PutStatic
| InvokeVirtual
| InvokeStatic
| InvokeSpecial
| NewInvokeSpecial
| InvokeInterface

[<Struct>]
type MethodHandle = { ReferenceKind : RefKind; ReferenceIndex : uint16 }

[<Struct>]
type DynamicInfo = {
    BootstrapMethodAttrIndex : uint16
    NameAndTypeIndex : uint16
}

type ConstantType =
| CUtf8 of string
| CInteger of int
| CFloat of float32
| CLong of int64
| CDouble of float
| CClass of Utf8Index
| CString of Utf8Index
| CFieldref of RefInfo
| CMethodref of RefInfo
| CInterfaceMethodref of RefInfo
| CNameAndType of NameAndType
| CMethodHandle of MethodHandle
| CMethodType of Utf8Index
| CDynamic of DynamicInfo
| CInvokeDynamic of DynamicInfo
| CModule of Utf8Index
| CPackage of Utf8Index
| Unknown of uint8

type ClassFile = {
    Magic : Magic
    MinorVersion : uint16
    MajorVersion : uint16
    ConstantPool : IReadOnlyDictionary<uint16, ConstantType>
    (*
    u2             access_flags;
    u2             this_class;
    u2             super_class;
    u2             interfaces_count;
    u2             interfaces[interfaces_count];
    u2             fields_count;
    field_info     fields[fields_count];
    u2             methods_count;
    method_info    methods[methods_count];
    u2             attributes_count;
    attribute_info attributes[attributes_count];*)
}