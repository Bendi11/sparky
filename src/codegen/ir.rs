use std::{rc::Rc, cell::RefCell};

use hashbrown::HashMap;

use crate::{Symbol, arena::Index, ast::IntegerWidth};

pub type SparkModuleId = Index<SparkModule>;
pub type TypeId = Index<Type>;
pub type FunctionId = Index<Function>;

/// Structure containing arenas holding all function definitions, 
/// types, etc.
#[derive(Clone, Debug)]
pub struct SparkCtx {
    types: Rc<RefCell<Type>>,
    modules: Rc<RefCell<SparkModule>>,
}

/// Structure containing type data plus a type ID that can be used to refer to the
/// type
#[derive(Clone, Debug)]
pub struct Type {
    data: TypeData,
    id: TypeId,
}

/// Function containing an entry basic block and argument data
#[derive(Clone, Debug)]
pub struct Function {
    id: FunctionId,
    name: Symbol,
}

/// A single type, either user-defined or predefined
#[derive(Clone, Debug)]
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
        fields: Vec<TypeId>,
        //Map of field names to their indices in the `fields` vec
        field_names: HashMap<Symbol, usize>,
    },
    Enum {
        name: Option<Symbol>,
        parts: Vec<TypeId>,
    },
    Alias(TypeId),
    Function(FunctionType),
}

/// A function's type including argument types, return type, and flags
#[derive(Clone, Debug)]
pub struct FunctionType {
    return_ty: TypeId,
    args: Vec<TypeId>,
}

/// Structure holding all definitions contained in a single module
#[derive(Clone, Debug)]
pub struct SparkModule {
    pub id: SparkModuleId,
    pub name: Symbol,
    pub functions: HashMap<Symbol, FunctionId>,
}
