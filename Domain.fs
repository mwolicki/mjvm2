module Domain

[<Struct>]
type  Magic = private { Magic:uint32 }
let magic value = if value <> 0xCAFEBABEu then failwithf "Invalid magic number (expected = 0xCAFEBABE, got = %X)!" value else { Magic = value }

[<Struct>]
type RefInfo = { ClassIndex:uint16; NameAndTypeIndex:uint16 }

type ConstantType =
| CUtf8
| CInteger of int
| CFloat of float32
| CLong of int64
| CDouble of float
| CClass
| CString
| CFieldref of RefInfo
| CMethodref of RefInfo
| CInterfaceMethodref of RefInfo
| CNameAndType
| CMethodHandle
| CMethodType
| CDynamic
| CInvokeDynamic
| CModule
| CPackage
| Unknown of uint8

type ClassFile = {
    Magic : Magic
    MinorVersion : uint16
    MajorVersion : uint16
    (*u2             constant_pool_count;
    cp_info        constant_pool[constant_pool_count-1];
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