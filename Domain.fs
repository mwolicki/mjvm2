module Domain
open System.Collections.Generic

[<Struct>]
type  Magic = private { Magic:uint32 }
let magic value = if value <> 0xCAFEBABEu then failwithf "Invalid magic number (expected = 0xCAFEBABE, got = %X)!" value else { Magic = value }

[<Struct>]
type RefInfo = { ClassIndex:uint16; NameAndTypeIndex:uint16 }

[<Struct>]
type ClassInfo = ClassInfo of uint16

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

[<System.Flags>]
type AccessFlag =
/// Declared public; may be accessed from outside its package.
| ACC_PUBLIC = 0x0001
/// Declared final; no subclasses allowed.
| ACC_FINAL = 0x0010
/// Treat superclass methods specially when invoked by the invokespecial instruction.
| ACC_SUPER = 0x0020
/// Is an interface, not a class.
| ACC_INTERFACE = 0x0200
/// Declared abstract; must not be instantiated.
| ACC_ABSTRACT = 0x0400
/// Declared synthetic; not present in the source code.
| ACC_SYNTHETIC = 0x1000
/// Declared as an annotation type.
| ACC_ANNOTATION = 0x2000
/// Declared as an enum type.
| ACC_ENUM = 0x4000
/// Is a module, not a class or interface.
| ACC_MODULE = 0x8000

[<Struct>]
type Version = {
    MinorVersion : uint16
    MajorVersion : uint16
 }

type ClassFile = {
    /// The magic item supplies the magic number identifying the class file format; it has the value 0xCAFEBABE.
    Magic : Magic
    /// The values of the minor_version and major_version items are the minor and major version numbers of this class file. Together, a major and a minor version number determine the version of the class file format. If a class file has major version number M and minor version number m, we denote the version of its class file format as M.m. Thus, class file format versions may be ordered lexicographically, for example, 1.5 < 2.0 < 2.1.
    Version : Version
    /// The constant_pool is a table of structures (§4.4) representing various string constants, class and interface names, field names, and other constants that are referred to within the ClassFile structure and its substructures. The format of each constant_pool table entry is indicated by its first "tag" byte.
    ConstantPool : IReadOnlyDictionary<uint16, ConstantType>
    /// The value of the access_flags item is a mask of flags used to denote access permissions to and properties of this class or interface. The interpretation of each flag, when set, is specified in Table 4.1-B.
    AccessFlags : AccessFlag
    /// The value of the this_class item must be a valid index into the constant_pool table. The constant_pool entry at that index must be a CONSTANT_Class_info structure (§4.4.1) representing the class or interface defined by this class file.
    ThisClass : ClassInfo
    /// For a class, the value of the super_class item either must be zero or must be a valid index into the constant_pool table. If the value of the super_class item is nonzero, the constant_pool entry at that index must be a CONSTANT_Class_info structure representing the direct superclass of the class defined by this class file. Neither the direct superclass nor any of its superclasses may have the ACC_FINAL flag set in the access_flags item of its ClassFile structure.
    /// 
    /// If the value of the super_class item is zero, then this class file must represent the class Object, the only class or interface without a direct superclass.
    /// 
    /// For an interface, the value of the super_class item must always be a valid index into the constant_pool table. The constant_pool entry at that index must be a CONSTANT_Class_info structure representing the class Object.
    SuperClass : ClassInfo option
    (*
    u2             interfaces_count;
    u2             interfaces[interfaces_count];
    u2             fields_count;
    field_info     fields[fields_count];
    u2             methods_count;
    method_info    methods[methods_count];
    u2             attributes_count;
    attribute_info attributes[attributes_count];*)
}