//! Module containing definitions for the intermediate representation
//! that the AST is lowered to during code generation

pub mod lower;

use std::collections::HashMap;

use crate::arena::{Index, Arena};
use num_bigint::BigInt;
use string_interner::symbol::SymbolU32 as Symbol;

use crate::{ast::{IntegerWidth, SymbolPath, PathIter, AstNode, NumberLiteral}, parse::token::Op};

pub type ModuleId = Index<Module>;

pub type FunId = Index<Fun>;
pub type TypeId = Index<Type>;
pub type VarId = Index<Var>;

/// A single variable declaration's data
#[derive(Clone, Debug)]
pub struct Var {
    /// The type of the variable
    pub ty: TypeId,
    /// The index of this variable in an [IRContext]
    pub id: VarId,
}

/// Structure holding both a type ID and the type data
#[derive(Clone, Debug)]
pub struct Type {
    pub id: TypeId,
    pub data: TypeData,
}

/// Structure containing all data needed to represent any type in the IR
#[derive(Clone, Debug)]
pub enum TypeData {
    /// An integer type with width and signededness
    Integer {
        /// If this is an unsigned integer type
        signed: bool,
        /// The width of the integer
        width: IntegerWidth,
    },
    /// A floating point number
    Float {
        /// Is this an f64 or f32
        doublewide: bool,
    },
    /// A single true or false type 
    Bool,
    /// A pointer to another type
    Pointer(TypeId),
    /// An array of one type
    Array {
        /// The element types of the array
        elements: TypeId,
        /// The length of the array
        len: u64,
    },
    /// A tuple combination of arbitrary types
    Tuple {
        /// A list of all types in the tuple
        elements: Vec<TypeId>,
    },
    /// A structure type mapping fields to more types
    Struct {
        /// The field names and types of this structure
        fields: HashMap<Symbol, TypeId>,
    },
    /// An enumeration containing one variant of many types
    Enum {
        /// The possible variant types
        variants: Vec<TypeId>,
    },
    /// A function type with arguments and return type
    Fun {
        /// The type of all arguments to the function
        args: Vec<TypeId>,
        /// The return type of the function
        return_ty: TypeId,
    },
    /// A zero-byte sized type that acts as a void type
    Unit,
    /// An alias to another type
    Alias(TypeId),
    /// A type indicating an internal compiler error
    Invalid,
}

/// An IR context containing all blocks, definitions, etc.
#[derive(Clone, Debug)]
pub struct IRContext {
    pub modules: Arena<Module>,
    pub funs: Arena<Fun>,
    pub types: Arena<Type>,
    pub vars: Arena<Var>,

    pub u_ids: [TypeId ; 4],
    pub i_ids: [TypeId ; 4],
    pub f32_id: TypeId,
    pub f64_id: TypeId,
    pub bool_id: TypeId,
    pub unit_id: TypeId,
    pub invalid_id: TypeId,
}

impl IRContext {
    /// Create a new IR context, populating the types arena with integeral types
    pub fn new() -> Self {
        let mut types = Arena::new();
        Self {
            u_ids: [
                types.insert_with(|id| Type {id, data: TypeData::Integer { width: IntegerWidth::Eight, signed: false } }),
                types.insert_with(|id| Type {id, data: TypeData::Integer { width: IntegerWidth::Sixteen, signed: false } }),
                types.insert_with(|id| Type {id, data: TypeData::Integer { width: IntegerWidth::ThirtyTwo, signed: false } }),
                types.insert_with(|id| Type {id, data: TypeData::Integer { width: IntegerWidth::SixtyFour, signed: false } }),
            ],
            i_ids: [
                types.insert_with(|id| Type {id, data: TypeData::Integer { width: IntegerWidth::Eight, signed: true } }),
                types.insert_with(|id| Type {id, data: TypeData::Integer { width: IntegerWidth::Sixteen, signed: true } }),
                types.insert_with(|id| Type {id, data: TypeData::Integer { width: IntegerWidth::ThirtyTwo, signed: true } }),
                types.insert_with(|id| Type {id, data: TypeData::Integer { width: IntegerWidth::SixtyFour, signed: true } }),
            ],
            f32_id: types.insert_with(|id| Type {id, data: TypeData::Float{doublewide: false}}),
            f64_id: types.insert_with(|id| Type {id, data: TypeData::Float{doublewide: true}}),
            bool_id: types.insert_with(|id| Type {id, data: TypeData::Bool}),
            unit_id: types.insert_with(|id| Type {id, data: TypeData::Unit}),
            invalid_id: types.insert_with(|id| Type {id, data: TypeData::Invalid}),
            types,
            modules: Arena::new(),
            funs: Arena::new(),
            vars: Arena::new(),
        }
    }
        
    /// Create a new variable entry in the arena and return an index to it
    pub fn new_var(&mut self, ty: TypeId) -> VarId {
        self.vars.insert_with(|id| Var {
            ty,
            id
        })
    }


    /// Create a new module with the given name
    pub fn new_module(&mut self, name: Symbol) -> ModuleId {
        self.modules.insert_with(|id| Module {
            id,
            children: HashMap::new(),
            imports: HashMap::new(),
            typedefs: HashMap::new(),
            funs: HashMap::new(),
            name
        })
    }
    
    /// Create a new empty function with the given name
    pub fn new_fun(&mut self, name: Symbol) -> FunId {
        let unit_id = self.unit_id;

        self.funs.insert_with(|id| Fun {
            name,
            args: Vec::new(),
            return_ty: unit_id,
            body: None,
            id,
        })
    }

    pub fn new_type(&mut self) -> TypeId {
        self.types.insert_with(|id| Type {
            id,
            data: TypeData::Unit
        })
    }
}

/// A single module containing multiple definitions 
#[derive(Clone, Debug)]
pub struct Module {
    /// The ID of this module
    pub id: ModuleId,
    /// A map of names to child modules
    pub children: HashMap<Symbol, ModuleId>,
    /// A map of all imports in the module
    pub imports: HashMap<Symbol, ModuleId>,
    /// A map of all typenames to their type data
    pub typedefs: HashMap<Symbol, TypeId>,
    /// A map of all function names to their data
    pub funs: HashMap<Symbol, FunId>,
    /// The name of the module
    pub name: Symbol,
}


impl IRContext {
    /// Get either a child module or an imported module of this module
    pub fn get_submodule(&self, id: ModuleId, name: Symbol) -> Option<ModuleId> {
        let module = &self.modules[id];
        match module.children.get(&name) {
            Some(moduleid) => Some(*moduleid),
            None => module.imports.get(&name).copied(),
        }
    }
    
    /// Get an imported or child from this module by path
    pub fn get_submodule_path(&self, id: ModuleId, path: &SymbolPath) -> Option<ModuleId> {
        self.get_module_impl(id, path.iter())
    }
    
    /// Get a submodule or import by path
    fn get_module_impl(&self, id: ModuleId, mut iter: PathIter) -> Option<ModuleId> {
        match iter.next() {
            Some(name) => {
                let id = self.get_submodule(id, name)?;
                self.get_module_impl(id, iter)
            }
            None => Some(id)
        }
    }
    
    /// Get a type by path from this module
    pub fn get_type(&self, id: ModuleId, path: &SymbolPath) -> Option<TypeId> {
        self.get_type_impl(id, path.iter())
    }
    
    /// Get a type from this module by path
    fn get_type_impl(&self, id: ModuleId, mut iter: PathIter) -> Option<TypeId> {
        if iter.len() == 1 {
            self.modules[id].typedefs.get(&iter.next().unwrap()).copied()
        } else {
            let submod = self.get_submodule(id, iter.next().unwrap())?;
            self.get_type_impl(submod, iter)
        }
    }

    /// Get a function by path from this module
    pub fn get_fun(&self, id: ModuleId, path: &SymbolPath) -> Option<FunId> {
        self.get_fun_impl(id, path.iter())
    }
    
    /// Get a function from this module by path
    fn get_fun_impl(&self, id: ModuleId, mut iter: PathIter) -> Option<FunId> {
        if iter.len() == 1 {
            self.modules[id].funs.get(&iter.next().unwrap()).copied()
        } else {
            let submod = self.get_submodule(id, iter.next().unwrap())?;
            self.get_fun_impl(submod, iter)
        }
    }

}

/// A single function with name, argument types, etc.
#[derive(Clone, Debug)]
pub struct Fun {
    /// ID of the function in the context
    pub id: FunId,
    /// The name of the function
    pub name: Symbol, 
    /// The argument types and optional names of the function
    pub args: Vec<(TypeId, Option<Symbol>)>,
    /// The return type of the function
    pub return_ty: TypeId,
    /// If the function is defined, this is the entry block of the function
    pub body: Option<Vec<Node>>,
}

/// A single expression or statement in the IR
#[derive(Clone, Debug)]
pub enum Node {
    /// Declare a variable of an explicit type
    VarDec {
        id: VarId, 
        ty: Type, 
        mutable: bool
    },
    /// Function address is being requested
    FunAccess(FunId),
    /// Variable accessed by name
    VarAccess(VarId),
    /// Accessing a field of a structure
    FieldAccess(Box<Node>, Symbol),
    /// Indexing using an expression
    Index {
        expr: Box<Node>,
        idx: Box<Node>,
    },
    Assign {
        src: Box<Node>,
        dest: Box<Node>
    },
    Bin(Box<Node>, Op, Box<Node>),
    Unary(Op, Box<Node>),
    /// Call a funtion value
    Call {
        fun_expr: Box<Node>,
        args: Vec<Node>,
    },
    If(Box<IfNode>),
    
    Phi(Box<Node>),
    Return(Box<Node>),
    Cast(TypeId, Box<Node>),
    NumberLiteral(NumberLiteral),
    StringLiteral(String),
    BooleanLiteral(bool),
    TupleLiteral(Vec<Node>),
    ArrayLiteral(Vec<Node>),
    Unit,
    
    Break,
    Continue,
    
    Loop(Vec<Node>),
    Block(Vec<Node>),
}

#[derive(Clone, Debug)]
pub struct IfNode {
    cond: Node,
    body: Vec<Node>,
    else_node: Option<Box<ElseNode>>,
}

#[derive(Clone, Debug)]
pub enum ElseNode {
    ElseIf(IfNode),
    Else(Vec<Node>)
}
