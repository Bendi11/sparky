//! Module containing definitions for structures representing type-lowered Intermediate
//! Representation created from an Abstract Syntax Tree

pub mod value;
pub mod types;

use crate::{ast::{IntegerWidth, FunFlags}, arena::{Interner, Index, Arena}, Symbol, util::{files::FileId, loc::Span}};

use self::{
    value::IrAnyValue,
    types::{IrType, fun::IrFunType, integer::IrIntegerType, float::IrFloatType}
};


/// An IR context containing arenas with all type definitons, function declarations / definitions,
/// and modules
pub struct IrContext {
    /// A container with all defined types 
    pub types: Interner<IrType>,
    /// All declared / defined functions
    pub funs: Arena<IrFun>,
}

/// ID referencing an [IrType] in an [IrContext]
pub type TypeId = Index<IrType>;
    
/// ID referencing an [IrBB] in an [IrBody]
pub type BBId = Index<IrBB>;

/// ID referencing an [IrVar] in an [IrBody]
pub type VarId = Index<IrVar>;
    
/// ID referencing an [IrFun] in an [IrContext]
pub type FunId = Index<IrFun>;

/// ID referencing an [IrType] that is an enum discriminant in an [IrType::Sum]
pub type DiscriminantId = Index<TypeId>;

/// A single basic block in the IR containing a list of statements
pub struct IrBB {
    /// A list of statements in the order they should execute
    pub stmts: Vec<IrStmt>,
    /// The terminator statement of this basic block
    pub terminator: IrTerminator,
}

/// A declared variable with type and name
pub struct IrVar {
    /// Type of the variable
    pub ty: TypeId,
    /// User-asigned name of the variable
    pub name: Symbol,
}

/// Function with source location information and optional body
pub struct IrFun {
    /// Name of the function, may be generated by the compiler
    pub name: Symbol,
    /// Function's signature
    pub ty: IrFunType,
    /// Source file that contains this function's definition
    pub file: FileId,
    /// Span in the source file of this function
    pub span: Span,
    /// Body of the function, if defined
    pub body: Option<IrBody>,
    /// Any extra flags of the function
    pub flags: FunFlags,
}

/// The body of a function, composed of multiple statements and basic blocks
pub struct IrBody {
    /// All basic blocks in the function containing multiple statements
    pub basic_blocks: Arena<IrBB>,
    /// Entry block of the body 
    pub entry: BBId,
    /// All local variable declarations
    pub vars: Arena<IrVar>,
    /// The parent function
    pub parent: FunId,
}

/// A statement that may terminate a basic block
pub enum IrTerminator {
    /// Exits the currently executing function
    Return(IrAnyValue),
    /// Jumps unconditionally to another basic block
    Jmp(BBId),
    /// Jumps conditionally
    JmpIf {
        /// Boolean-valued condtion being checked
        condition: IrAnyValue,
        /// Basic block to jump to if the condition evaluates to true
        if_true: BBId,
        /// Basic block to jump to otherwise
        if_false: BBId,
    },
    /// Matches against an enum's discriminant
    JmpMatch {
        /// Variant being tested 
        variant: IrAnyValue,
        /// List of checked discriminants by their indices
        discriminants: Vec<(DiscriminantId, BBId)>,
        /// Default jump
        default_jmp: BBId,
    },
}

/// A single statement in the Intermediate Representation
pub enum IrStmt {
    /// Store a value in a variable
    Store {
        /// The variable to store into
        var: VarId,
        /// Value to store in variable
        val: IrAnyValue
    }
}

impl IrContext {
    pub const I8: TypeId = unsafe { TypeId::from_raw(0) };
    pub const I16: TypeId = unsafe { TypeId::from_raw(1) };
    pub const I32: TypeId = unsafe { TypeId::from_raw(2) };
    pub const I64: TypeId = unsafe { TypeId::from_raw(3) };
    pub const U8: TypeId = unsafe { TypeId::from_raw(4) };
    pub const U16: TypeId = unsafe { TypeId::from_raw(5) };
    pub const U32: TypeId = unsafe { TypeId::from_raw(6) };
    pub const U64: TypeId = unsafe { TypeId::from_raw(7) };

    pub const BOOL: TypeId = unsafe { TypeId::from_raw(8) };
    pub const UNIT: TypeId = unsafe { TypeId::from_raw(9) };

    pub const F32: TypeId = unsafe { TypeId::from_raw(10) };
    pub const F64: TypeId = unsafe { TypeId::from_raw(11) };

    /// Create a new `IRContext` with primitive types defined
    pub fn new() -> Self {
        let mut types = Interner::<IrType>::new();

        types.insert(IrIntegerType { signed: true, width: IntegerWidth::Eight }.into());    
        types.insert(IrIntegerType { signed: true, width: IntegerWidth::Sixteen }.into());
        types.insert(IrIntegerType { signed: true, width: IntegerWidth::ThirtyTwo }.into());
        types.insert(IrIntegerType { signed: true, width: IntegerWidth::SixtyFour }.into());
        
        types.insert(IrIntegerType { signed: false, width: IntegerWidth::Eight }.into());    
        types.insert(IrIntegerType { signed: false, width: IntegerWidth::Sixteen }.into());
        types.insert(IrIntegerType { signed: false, width: IntegerWidth::ThirtyTwo }.into());
        types.insert(IrIntegerType { signed: false, width: IntegerWidth::SixtyFour }.into());
        
        types.insert(IrType::Bool);
        types.insert(IrType::Unit);
    
        types.insert(IrFloatType { doublewide: false }.into());
        types.insert(IrFloatType { doublewide: true }.into());

        Self {
            types,
            funs: Arena::new(),
        }
    }
    
    /// Get the [TypeId] of an integer type with the given width and signededness
    pub const fn itype(signed: bool, width: IntegerWidth) -> TypeId {
        match (signed, width) {
            (true, IntegerWidth::Eight) => Self::I8,
            (true, IntegerWidth::Sixteen) => Self::I16,
            (true, IntegerWidth::ThirtyTwo) => Self::I32,
            (true, IntegerWidth::SixtyFour) => Self::I64,
            
            (false, IntegerWidth::Eight) => Self::U8,
            (false, IntegerWidth::Sixteen) => Self::U16,
            (false, IntegerWidth::ThirtyTwo) => Self::U32,
            (false, IntegerWidth::SixtyFour) => Self::U64,
        }
    }
}
