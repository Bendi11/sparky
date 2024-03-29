//! Module containing definitions for structures representing type-lowered Intermediate
//! Representation created from an Abstract Syntax Tree

pub mod lower;
pub mod types;
pub mod value;

use std::ops::IndexMut;

use hashbrown::HashSet;

use crate::{
    arena::{Arena, Index, Interner},
    ast::{FunFlags, IntegerWidth},
    util::{files::FileId, loc::Span},
    Symbol,
};

use self::{
    types::{FunType, IrFloatType, IrIntegerType, IrType},
    value::IrExpr,
};

/// An IR context containing arenas with all type definitons, function declarations / definitions,
/// and modules
pub struct IrContext {
    /// A container with all defined types
    pub types: Interner<IrType>,
    /// All declared / defined functions
    pub funs: Arena<IrFun>,
    /// All basic blocks in the program containing statements
    pub bbs: Arena<IrBB>,
    /// All variables in the program
    pub vars: Arena<IrVar>,
    /// All global values in the program
    pub globals: Arena<IrGlobal>,
}

/// ID referencing an [IrType] in an [IrContext]
pub type TypeId = Index<IrType>;

/// ID referencing an [IrBB] in an [IrBody]
pub type BBId = Index<IrBB>;

/// ID referencing an [IrVar] in an [IrBody]
pub type VarId = Index<IrVar>;

/// ID referencing an [IrFun] in an [IrContext]
pub type FunId = Index<IrFun>;

/// ID referencing an [IrType::Type] that is an enum discriminant in an [Ir::Sum]
pub type DiscriminantId = TypeId;

/// ID referencing an [IrGlobal]
pub type GlobalId = Index<IrGlobal>;

/// A single basic block in the IR containing a list of statements
#[derive(Clone, Debug)]
pub struct IrBB {
    /// A list of statements in the order they should execute
    pub stmts: Vec<IrStmt>,
    /// The terminator statement of this basic block
    pub terminator: IrTerminator,
}

/// A declared variable with type and name
#[derive(Clone, Debug)]
pub struct IrVar {
    /// Type of the variable
    pub ty: TypeId,
    /// User-asigned name of the variable
    pub name: Symbol,
}

/// A global variable
#[derive(Clone, Debug)]
pub struct IrGlobal {
    /// Type of the global value
    pub ty: TypeId,
    /// Name of this global
    pub name: Symbol,
}

/// Function with source location information and optional body
pub struct IrFun {
    /// Name of the function, may be generated by the compiler
    pub name: Symbol,
    /// Function's signature
    pub ty: FunType,
    /// ID of the function type
    pub ty_id: TypeId,
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
#[derive(Clone, Debug)]
pub struct IrBody {
    /// Entry block of the body
    pub entry: BBId,
    /// The parent function
    pub parent: FunId,
    /// A list of argument allocations for each parameter
    pub args: Vec<Option<VarId>>,
}

/// A statement that may terminate a basic block
#[derive(Clone, Debug)]
pub enum IrTerminator {
    /// Exits the currently executing function
    Return(IrExpr),
    /// Jumps unconditionally to another basic block
    Jmp(BBId),
    /// Jumps conditionally
    JmpIf {
        /// Boolean-valued condtion being checked
        condition: IrExpr,
        /// Basic block to jump to if the condition evaluates to true
        if_true: BBId,
        /// Basic block to jump to otherwise
        if_false: BBId,
    },
    /// Matches against an enum's discriminant
    JmpMatch {
        /// Variant being tested
        variant: IrExpr,
        /// List of checked discriminants by their indices
        discriminants: Vec<(DiscriminantId, BBId)>,
        /// Default jump
        default_jmp: BBId,
    },
    /// Internal compiler usage
    Invalid,
}

/// A single statement in the IR, an instruction that produces no value
#[derive(Clone, Debug)]
pub struct IrStmt {
    pub span: Span,
    pub kind: IrStmtKind,
}

/// A single statement in the Intermediate Representation
#[derive(Clone, Debug)]
pub enum IrStmtKind {
    /// Allocate space for the given variable
    VarLive(VarId),
    /// Store a value in a variable
    Store {
        /// The variable to store into
        var: VarId,
        /// Value to store in variable
        val: IrExpr,
    },
    Write {
        //// Expression that must be of pointer type
        ptr: IrExpr,
        /// Value to write to the pointer
        val: IrExpr,
    },
    /// Call a function directly
    Call { fun: FunId, args: Vec<IrExpr> },
    /// Execute the given expression for side effects
    Exec(IrExpr),
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

    pub const INVALID: TypeId = unsafe { TypeId::from_raw(12) };

    pub const ISIZE: TypeId = unsafe { TypeId::from_raw(13) };
    pub const USIZE: TypeId = unsafe { TypeId::from_raw(14) };

    pub const CHAR: TypeId = unsafe { TypeId::from_raw(15) };

    /// Create a new `IRContext` with primitive types defined
    pub fn new() -> Self {
        let mut types = Interner::<IrType>::new();

        types.insert(
            IrType::Integer(IrIntegerType {
                signed: true,
                width: IntegerWidth::Eight,
            })
            .into(),
        );
        types.insert(
            IrType::Integer(IrIntegerType {
                signed: true,
                width: IntegerWidth::Sixteen,
            })
            .into(),
        );
        types.insert(
            IrType::Integer(IrIntegerType {
                signed: true,
                width: IntegerWidth::ThirtyTwo,
            })
            .into(),
        );
        types.insert(
            IrType::Integer(IrIntegerType {
                signed: true,
                width: IntegerWidth::SixtyFour,
            })
            .into(),
        );

        types.insert(
            IrType::Integer(IrIntegerType {
                signed: false,
                width: IntegerWidth::Eight,
            })
            .into(),
        );
        types.insert(
            IrType::Integer(IrIntegerType {
                signed: false,
                width: IntegerWidth::Sixteen,
            })
            .into(),
        );
        types.insert(
            IrType::Integer(IrIntegerType {
                signed: false,
                width: IntegerWidth::ThirtyTwo,
            })
            .into(),
        );
        types.insert(
            IrType::Integer(IrIntegerType {
                signed: false,
                width: IntegerWidth::SixtyFour,
            })
            .into(),
        );

        types.insert(IrType::Bool);
        types.insert(IrType::Unit);

        types.insert(IrType::Float(IrFloatType { doublewide: false }).into());
        types.insert(IrType::Float(IrFloatType { doublewide: true }).into());

        types.insert(IrType::Invalid);

        types.insert(IrType::Integer(IrIntegerType {
            width: IntegerWidth::PtrSize,
            signed: true,
        }));
        types.insert(IrType::Integer(IrIntegerType {
            width: IntegerWidth::PtrSize,
            signed: false,
        }));

        types.insert(IrType::Char);

        Self {
            types,
            funs: Arena::new(),
            bbs: Arena::new(),
            vars: Arena::new(),
            globals: Arena::new(),
        }
    }

    /// Get a human-readable type name for the given type
    #[inline]
    pub fn typename(&self, ty: TypeId) -> TypenameFormatter<'_> {
        TypenameFormatter { ctx: self, ty }
    }

    /// Get the [TypeId] of an integer type with the given width and signededness
    pub const fn itype(signed: bool, width: IntegerWidth) -> TypeId {
        match (signed, width) {
            (true, IntegerWidth::Eight) => Self::I8,
            (true, IntegerWidth::Sixteen) => Self::I16,
            (true, IntegerWidth::ThirtyTwo) => Self::I32,
            (true, IntegerWidth::SixtyFour) => Self::I64,
            (true, IntegerWidth::PtrSize) => Self::ISIZE,

            (false, IntegerWidth::Eight) => Self::U8,
            (false, IntegerWidth::Sixteen) => Self::U16,
            (false, IntegerWidth::ThirtyTwo) => Self::U32,
            (false, IntegerWidth::SixtyFour) => Self::U64,
            (false, IntegerWidth::PtrSize) => Self::USIZE,
        }
    }

    /// Unwrap any type aliases to get a type that is guranteed to not be an alias
    pub fn unwrap_alias(&self, ty: TypeId) -> TypeId {
        match &self[ty] {
            IrType::Alias { ty, .. } => self.unwrap_alias(*ty),
            _ => ty,
        }
    }

    /// Create a new basic block with invalid terminator and return the ID
    pub fn bb(&mut self) -> BBId {
        self.bbs.insert(IrBB {
            stmts: vec![],
            terminator: IrTerminator::Invalid,
        })
    }
}

/// Structure for more efficiently formatting typename strings via a std::fmt::Display
/// implementation avoiding multiple string allocations
pub struct TypenameFormatter<'ctx> {
    ctx: &'ctx IrContext,
    ty: TypeId,
}

impl<'ctx> TypenameFormatter<'ctx> {
    /// Create a new formatter for the given type ID using the same shared context
    const fn create(&self, ty: TypeId) -> Self {
        Self { ctx: self.ctx, ty }
    }
}

impl<'ctx> std::fmt::Display for TypenameFormatter<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ctx[self.ty] {
            IrType::Integer(IrIntegerType { signed, width }) => write!(
                f,
                "{}",
                match (signed, width) {
                    (true, IntegerWidth::Eight) => "i8",
                    (true, IntegerWidth::Sixteen) => "i16",
                    (true, IntegerWidth::ThirtyTwo) => "i32",
                    (true, IntegerWidth::SixtyFour) => "i64",
                    (true, IntegerWidth::PtrSize) => "isz",

                    (false, IntegerWidth::Eight) => "u8",
                    (false, IntegerWidth::Sixteen) => "u16",
                    (false, IntegerWidth::ThirtyTwo) => "u32",
                    (false, IntegerWidth::SixtyFour) => "u64",
                    (false, IntegerWidth::PtrSize) => "usz",
                }
            ),
            IrType::Bool => write!(f, "bool"),
            IrType::Char => write!(f, "char"),
            IrType::Unit => write!(f, "()"),
            IrType::Sum(sum) => {
                for variant in sum.iter() {
                    write!(f, "{} | ", self.create(*variant))?;
                }
                Ok(())
            }
            IrType::Float(IrFloatType { doublewide }) => write!(
                f,
                "{}",
                match doublewide {
                    true => "f64",
                    false => "f32",
                }
            ),
            IrType::Alias { name, .. } => write!(f, "{}", name),
            IrType::Array(element, len) => write!(f, "[{}]{}", len, self.create(*element)),
            IrType::Struct(structure) => {
                write!(f, "{{")?;
                for field in structure.fields.iter() {
                    write!(f, "{} {},", self.create(field.ty), field.name)?;
                }
                write!(f, "}}")
            }
            IrType::Ptr(ty) => write!(f, "*{}", self.create(*ty)),
            IrType::Fun(fun) => {
                write!(f, "fun (")?;
                for (arg_ty, arg_name) in fun.params.iter() {
                    write!(
                        f,
                        "{} {}, ",
                        self.create(*arg_ty),
                        arg_name.unwrap_or(Symbol::from(""))
                    )?;
                }

                write!(f, ") -> {}", self.create(fun.return_ty))
            }
            IrType::Invalid => write!(f, "INVALID"),
        }
    }
}

impl std::ops::Index<TypeId> for IrContext {
    type Output = IrType;
    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[index]
    }
}

impl std::ops::Index<FunId> for IrContext {
    type Output = IrFun;
    fn index(&self, index: FunId) -> &Self::Output {
        &self.funs[index]
    }
}
impl IndexMut<FunId> for IrContext {
    fn index_mut(&mut self, index: FunId) -> &mut Self::Output {
        &mut self.funs[index]
    }
}

impl std::ops::Index<VarId> for IrContext {
    type Output = IrVar;
    fn index(&self, index: VarId) -> &Self::Output {
        &self.vars[index]
    }
}
impl IndexMut<VarId> for IrContext {
    fn index_mut(&mut self, index: VarId) -> &mut Self::Output {
        &mut self.vars[index]
    }
}

impl std::ops::Index<BBId> for IrContext {
    type Output = IrBB;
    fn index(&self, index: BBId) -> &Self::Output {
        &self.bbs[index]
    }
}
impl IndexMut<BBId> for IrContext {
    fn index_mut(&mut self, index: BBId) -> &mut Self::Output {
        &mut self.bbs[index]
    }
}

impl std::ops::Index<GlobalId> for IrContext {
    type Output = IrGlobal;
    fn index(&self, index: GlobalId) -> &Self::Output {
        &self.globals[index]
    }
}
impl IndexMut<GlobalId> for IrContext {
    fn index_mut(&mut self, index: GlobalId) -> &mut Self::Output {
        &mut self.globals[index]
    }
}

impl std::fmt::Display for IrContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_bb(
            ctx: &IrContext,
            f: &mut std::fmt::Formatter<'_>,
            bb: BBId,
            indent: usize,
            written: &mut HashSet<BBId>,
        ) -> std::fmt::Result {
            if written.contains(&bb) {
                return Ok(());
            } else {
                written.insert(bb);
            }

            writeln!(
                f,
                "{} BB {}",
                std::iter::repeat(' ').take(indent * 2).collect::<String>(),
                bb
            )?;
            let indented = std::iter::repeat(' ')
                .take(indent + 1 * 2)
                .collect::<String>();
            for inst in ctx[bb].stmts.iter() {
                writeln!(
                    f,
                    "{}{}",
                    indented,
                    match &inst.kind {
                        IrStmtKind::Exec(inst) => format!("EXEC {:?}", inst.kind),
                        IrStmtKind::VarLive(v) =>
                            format!("VARLIVE {} ({})", ctx[*v].name, ctx.typename(ctx[*v].ty)),
                        IrStmtKind::Store { var, val } => format!(
                            "STORE {:?} -> {} ({})",
                            val.kind, ctx[*var].name, ctx[*var].ty
                        ),
                        IrStmtKind::Write { ptr, val } =>
                            format!("WRITE {:?} -> {:?}", ptr.kind, val.kind),
                        IrStmtKind::Call { fun, args } => format!(
                            "CALL {} ({:?})",
                            ctx[*fun].name,
                            args.iter()
                                .map(|arg| format!("{:?}", arg.kind))
                                .collect::<Vec<_>>()
                        ),
                    }
                )?;
            }

            writeln!(
                f,
                "{}{}",
                indented,
                match &ctx[bb].terminator {
                    IrTerminator::Return(v) => format!("RETURN {:?}", v.kind),
                    IrTerminator::Jmp(bb) => format!("JMP {}", bb),
                    IrTerminator::JmpIf {
                        condition,
                        if_true,
                        if_false,
                    } => format!(
                        "JMPIF {:?} -> {} else {}",
                        condition.kind, if_true, if_false
                    ),
                    IrTerminator::JmpMatch {
                        variant,
                        discriminants,
                        default_jmp,
                    } => format!(
                        "JMPMATCH {:?} -> {}else {}",
                        variant.kind,
                        discriminants
                            .iter()
                            .map(|(v, bb)| format!("{}{} -> {}\n", indented, ctx.typename(*v), bb))
                            .collect::<String>(),
                        default_jmp,
                    ),
                    IrTerminator::Invalid => "INVALID".to_owned(),
                }
            )?;

            match &ctx[bb].terminator {
                IrTerminator::Jmp(bb) => fmt_bb(ctx, f, *bb, indent + 1, written),
                IrTerminator::JmpIf {
                    condition: _,
                    if_true,
                    if_false,
                } => {
                    fmt_bb(ctx, f, *if_true, indent + 1, written)?;
                    fmt_bb(ctx, f, *if_false, indent + 1, written)
                }
                IrTerminator::JmpMatch {
                    variant: _,
                    discriminants,
                    default_jmp,
                } => {
                    for (_, bb) in discriminants {
                        fmt_bb(ctx, f, *bb, indent + 1, written)?;
                    }
                    fmt_bb(ctx, f, *default_jmp, indent, written)
                }
                _ => Ok(()),
            }
        }

        let mut written = HashSet::new();
        for fun in self.funs.indices() {
            writeln!(
                f,
                "{} {} [{:?}] in file {}",
                self.typename(self[fun].ty_id),
                self[fun].name,
                self[fun].flags,
                self[fun].file
            )?;
            if let Some(body) = self[fun].body.as_ref() {
                fmt_bb(self, f, body.entry, 0, &mut written)?;
            }
        }

        Ok(())
    }
}
