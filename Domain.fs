module Domain
open System
open System.Collections.Generic


[<Flags>]
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


module Lower = 
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

    [<Struct>]
    type Version = {
        MinorVersion : uint16
        MajorVersion : uint16
     }

     type AttributeInfo = {
        /// For all attributes, the attribute_name_index item must be a valid unsigned 16-bit index into the constant pool of the class. The constant_pool entry at attribute_name_index must be a CONSTANT_Utf8_info structure (§4.4.7) representing the name of the attribute.
        AttributeNameIndex : Utf8Index
        /// The value of the attribute_length item indicates the length of the subsequent information in bytes. The length does not include the initial six bytes that contain the attribute_name_index and attribute_length items.
        Info : ReadOnlyMemory<byte>
    }

     type FieldInfo = {
        /// The value of the access_flags item is a mask of flags used to denote access permission to and properties of this field. The interpretation of each flag, when set, is specified in Table 4.5-A.
        AccessFlags : AccessFlag
        /// The value of the name_index item must be a valid index into the constant_pool table. The constant_pool entry at that index must be a CONSTANT_Utf8_info structure (§4.4.7) which represents a valid unqualified name denoting a field (§4.2.2).
        NameIndex : Utf8Index
        /// The value of the descriptor_index item must be a valid index into the constant_pool table. The constant_pool entry at that index must be a CONSTANT_Utf8_info structure (§4.4.7) which represents a valid field descriptor (§4.3.2).
        DescriptorIndex : Utf8Index
        /// Each value of the attributes table must be an attribute_info structure (§4.7).
        AttributeInfo : AttributeInfo list
     }

      type MethodInfo = {
        /// The value of the access_flags item is a mask of flags used to denote access permission to and properties of this method. The interpretation of each flag, when set, is specified in Table 4.6-A.
        AccessFlags : AccessFlag
        /// The value of the name_index item must be a valid index into the constant_pool table. The constant_pool entry at that index must be a CONSTANT_Utf8_info structure (§4.4.7) representing either a valid unqualified name denoting a method (§4.2.2), or (if this method is in a class rather than an interface) the special method name <init>, or the special method name <clinit>.
        NameIndex : Utf8Index
        /// The value of the descriptor_index item must be a valid index into the constant_pool table. The constant_pool entry at that index must be a CONSTANT_Utf8_info structure representing a valid method descriptor (§4.3.3). Furthermore:
        /// 
        /// * If this method is in a class rather than an interface, and the name of the method is <init>, then the descriptor must denote a void method.
        /// * If the name of the method is <clinit>, then the descriptor must denote a void method, and, in a class file whose version number is 51.0 or above, a method that takes no arguments.
        DescriptorIndex : Utf8Index
        /// Each value of the attributes table must be an attribute_info structure (§4.7).
        AttributeInfo : AttributeInfo list
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
        /// Each value in the interfaces array must be a valid index into the constant_pool table. The constant_pool entry at each value of interfaces[i], where 0 ≤ i < interfaces_count, must be a CONSTANT_Class_info structure representing an interface that is a direct superinterface of this class or interface type, in the left-to-right order given in the source for the type.
        Interfaces : ClassInfo list
        /// Each value in the fields table must be a field_info structure (§4.5) giving a complete description of a field in this class or interface. The fields table includes only those fields that are declared by this class or interface. It does not include items representing fields that are inherited from superclasses or superinterfaces.
        Fields : FieldInfo list
        /// Each value in the methods table must be a method_info structure (§4.6) giving a complete description of a method in this class or interface. If neither of the ACC_NATIVE and ACC_ABSTRACT flags are set in the access_flags item of a method_info structure, the Java Virtual Machine instructions implementing the method are also supplied.
        /// 
        /// The method_info structures represent all methods declared by this class or interface type, including instance methods, class methods, instance initialization methods (§2.9.1), and any class or interface initialization method (§2.9.2). The methods table does not include items representing methods that are inherited from superclasses or superinterfaces.
        Methods : MethodInfo list
        /// Each value of the attributes table must be an attribute_info structure (§4.7).
        Attributes : AttributeInfo list
    }

module Higher =
    [<Struct>]
    type ClassName = ClassName of string

    [<RequireQualifiedAccess>]
    type FieldDescriptor = 
    /// B	byte	signed byte
    | Byte
    /// C	char	Unicode character code point in the Basic Multilingual Plane, encoded with UTF-16
    | Char
    /// D	double	double-precision floating-point value
    | Double
    /// F	float	single-precision floating-point value
    | Float
    /// I	int	integer
    | Integer
    /// J	long	long integer
    | Long
    /// L ClassName ;	reference	an instance of class ClassName
    | Reference of ClassName
    /// S	short	signed short
    | Short
    /// Z	boolean	true or false
    | Boolean
    /// [	reference	one array dimension
    | Array of FieldDescriptor

     type AttributeInfo = {
        Name : string
        Info : ReadOnlyMemory<byte>
    }


    type FieldInfo = {
        AccessFlags : AccessFlag
        Name : string
        Descriptor : FieldDescriptor
        Attributes : AttributeInfo list
     }

    type ClassFile = {
        AccessFlags : AccessFlag
        ThisClass : string
        SuperClass : string option
        Interfaces : string list
        Fields : FieldInfo list
    }


