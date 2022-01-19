use std::{rc::Rc, cell::RefCell};

use hashbrown::HashMap;
use quickscope::ScopeMap;

use crate::{Symbol, arena::{Index, Interner, Arena}, ast::IntegerWidth};

pub type TypeId = Index<Type>;
pub type FunId = Index<Function>;
pub type ModId = Index<SparkModule>;
pub type DefId = Index<SparkDef>;

/// Structure containing arenas holding all function definitions, 
/// types, etc.
#[derive(Clone,)]
pub struct SparkCtx {
    types: Interner<Type>,
    modules: Arena<SparkModule>,
    defs: Arena<SparkDef>,
    root_module: ModId,
}

impl SparkCtx {
    pub const I8:  TypeId = unsafe { TypeId::from_raw(0) };
    pub const I16: TypeId = unsafe { TypeId::from_raw(1) };
    pub const I32: TypeId = unsafe { TypeId::from_raw(2) };
    pub const I64: TypeId = unsafe { TypeId::from_raw(3) };

    pub const U8:  TypeId = unsafe { TypeId::from_raw(4) };
    pub const U16: TypeId = unsafe { TypeId::from_raw(5) };
    pub const U32: TypeId = unsafe { TypeId::from_raw(6) };
    pub const U64: TypeId = unsafe { TypeId::from_raw(7) };

    pub const F32:  TypeId = unsafe { TypeId::from_raw(8) };
    pub const F64:  TypeId = unsafe { TypeId::from_raw(9) };
    pub const BOOL: TypeId = unsafe { TypeId::from_raw(10) };
    pub const UNIT: TypeId = unsafe { TypeId::from_raw(11) };
    pub const INVALID: TypeId = unsafe { TypeId::from_raw(12) };

    pub fn new() -> Self {
        let mut types = Interner::new();
        types.insert_with(|id| Type { id, data: TypeData::Integer { width: IntegerWidth::Eight, signed: true}});
        let mut modules = Arena::new();
        let root_module = modules.insert_with(|id| SparkModule { id, name: Symbol::from("root"), defs: ScopeMap::new()});

        Self {
            types,
            modules,
            defs: Arena::new(),
            root_module,
        }
    }
}

/// Structure containing type data plus a type ID that can be used to refer to the
/// type
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Type {
    data: TypeData,
    id: TypeId,
}

/// Function containing an entry basic block and argument data
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Function {
    id: FunId,
    name: Symbol,
}

/// A single type, either user-defined or predefined
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeData {
    Integer {
        signed: bool,
        width: IntegerWidth,
    },
    Float {
        doublewide: bool,
    },
    Bool,
    Unit,
    Pointer(TypeId),
    Array {
        element: TypeId,
        len: u64,
    },
    Tuple(Vec<TypeId>),
    Struct {
        //Prevents interning from seeing two structure types as different
        name: Option<Symbol>,
        fields: Vec<(TypeId, Option<Symbol>)>,
    },
    Enum {
        name: Option<Symbol>,
        parts: Vec<TypeId>,
    },
    Alias(TypeId),
    Function(FunctionType),
}

/// A function's type including argument types, return type, and flags
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    return_ty: TypeId,
    args: Vec<TypeId>,
}


/// Structure holding all definitions contained in a single module
#[derive(Clone)]
pub struct SparkModule {
    id: ModId,
    pub name: Symbol,
    pub defs: ScopeMap<Symbol, DefId>,
}

/// Class containing a single definition in a module
#[derive(Clone)]
pub struct SparkDef {
    id: DefId,
}

/// A single definition in the 
#[derive(Clone, Copy)]
pub enum DefData {
    TypeDef(TypeId),
    FunDef(FunId),
    Import(DefId),
}
