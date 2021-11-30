//! Abstract syntax tree structures, the first representation of the program made by the compiler



/// A node in an Abstract Syntax Tree
#[derive(Clone, Debug)]
pub enum AstNode {
    FunDecl
}

/// All types in the [AstNode] enumeration are represented by the `UnresolvedType` type, as 
/// user-defined types are resolved when lowering the AST to IR
#[derive(Clone, Debug)]
pub enum UnresolvedType {
    Integer {
        /// How large in bits is the integer type
        width: IntegerWidth,
        /// Is the integer type signed or unsigned
        signed: bool,
    },
    Float {
        /// If this is an f32 or an f64
        doublewide: bool,
    },
    Pointer(Box<UnresolvedType>),
    Array {
        elements: Box<UnresolvedType>, 
        len: u64
    },
    /// User-defined identifier
    UserDefined {
        /// The name of the user-defined type
        name: String,
        /// Names of any generic type arguments for the type
        generic_args: Vec<String>,
    },
}

/// Enumeration for all possible integer bit widths in the [UnresolvedType] enum
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum IntegerWidth {
    Eight = 8,
    Sixteen = 16,
    ThirtyTwo = 32,
    SixtyFour = 64
}