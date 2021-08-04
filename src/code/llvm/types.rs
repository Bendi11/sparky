//! The `types` module provides implementations for the [Compiler] struct for finding types, functions, and converting AST types to
//! LLVM types

use inkwell::{module::Linkage, values::GlobalValue, AddressSpace};

use crate::{
    ast::{AstPos, Attributes},
    code::ns::Path,
    lex::Pos,
};

use super::*;

impl<'a, 'c> Compiler<'a, 'c> {
    /// Get a struct type from the given path
    pub fn get_struct(&self, name: impl AsRef<str>) -> Option<Container> {
        let path: Path = name.as_ref().parse().unwrap();
        match self.current_ns.get().get_struct(path.clone()) {
            Some(s) => Some(s),
            None => self.root.get_struct(path),
        }
    }

    /// Get a union type from the given path
    pub fn get_union(&self, name: impl AsRef<str>) -> Option<Container> {
        let path: Path = name.as_ref().parse().unwrap();
        match self.current_ns.get().get_union(path.clone()) {
            Some(s) => Some(s),
            None => self.root.get_union(path),
        }
    }

    /// Get a typedef'd type from the given path
    pub fn get_typedef(&self, name: impl AsRef<str>) -> Option<Type> {
        let path: Path = name.as_ref().parse().unwrap();
        match self.current_ns.get().get_typedef(path.clone()) {
            Some(s) => Some(s),
            None => self.root.get_typedef(path),
        }
    }

    /// Get a struct type from the given path
    pub fn get_fun(&self, name: impl AsRef<str>) -> Option<(FunctionValue<'c>, FunProto)> {
        let path: Path = name.as_ref().parse().unwrap();
        match self.current_ns.get().get_fun(path.clone()) {
            Some(s) => Some(s),
            None => self.root.get_fun(path),
        }
    }

    /// Get a constant value from the given path
    pub fn get_const(&self, name: impl AsRef<str>) -> Option<(GlobalValue<'c>, Type)> {
        let path: Path = name.as_ref().parse().unwrap();
        match self.current_ns.get().get_const(path.clone()) {
            Some(s) => Some(s),
            None => self.root.get_const(path),
        }
    }

    /// Enter a new namespace or create one if the namespace doesn't exist
    pub fn enter_ns(&self, ns: &Path) {
        let iter = ns.parts();
        for name in iter {
            let contains = self.current_ns.get().nested.borrow().contains_key(name);
            match contains {
                true => self
                    .current_ns
                    .set(self.current_ns.get().get_ns(name.parse().unwrap()).unwrap()),
                false => {
                    let ns = self.arena.alloc(Ns::new_empty(name.clone()));
                    self.current_ns.get().add_ns(ns);
                    self.current_ns.set(ns);
                }
            }
        }
    }

    /// Exit a certain amount of namepsaces by going one namespace up
    pub fn exit_ns(&self, depth: usize) {
        for _ in 0..depth {
            match self.current_ns.get().parent.borrow().as_ref() {
                Some(parent) => self.current_ns.set(*parent),
                None => break,
            }
        }
    }

    /// Get all type definitions and track them as opaque struct types
    fn get_opaques(&self, ast: Vec<AstPos>) -> Vec<AstPos> {
        for node in ast.iter() {
            match node.ast() {
                Ast::StructDec(container) => {
                    trace!(
                        "Generating initial opaque llvm type for struct type {}",
                        self.current_ns.get().qualify(&container.name)
                    );
                    let name = self.current_ns.get().qualify(&container.name).to_string();
                    let ty = self.ctx.opaque_struct_type(&name);
                    self.struct_types.borrow_mut()[container.typeid] = ty;
                    self.current_ns
                        .get()
                        .struct_types
                        .borrow_mut()
                        .insert(container.name.clone(), container.clone());
                }
                Ast::UnionDec(container) => {
                    trace!(
                        "Generating initial opaque llvm type for union type {}",
                        self.current_ns.get().qualify(&container.name)
                    );
                    let name = self.current_ns.get().qualify(&container.name).to_string();
                    let ty = self.ctx.opaque_struct_type(&name);
                    self.struct_types.borrow_mut()[container.typeid] = ty;
                    self.current_ns
                        .get()
                        .union_types
                        .borrow_mut()
                        .insert(container.name.clone(), container.clone());
                }
                Ast::Ns(ns, stmts) => {
                    trace!("Entering namespace {}", ns);
                    self.enter_ns(ns);
                    self.get_opaques(stmts.clone());
                    self.exit_ns(ns.count());
                }

                _ => (),
            }
        }
        ast
    }

    /// Generate code for a function prototype
    pub(super) fn gen_fun_proto(
        &self,
        proto: &FunProto,
        pos: &Pos,
    ) -> Result<FunctionValue<'c>, String> {
        let qualified = match proto.attrs.contains(Attributes::EXT) {
            false => self.current_ns.get().qualify(&proto.name).to_string(),
            true => proto.name.clone(),
        };
        if self.module.get_function(qualified.as_str()).is_some() {
            return Err(format!("Function {} defined twice", qualified));
        }

        let proto_clone = proto.clone();
        if proto.ret == Type::Void {
            let fun = self.module.add_function(
                &qualified,
                self.ctx.void_type().fn_type(
                    proto
                        .args
                        .iter()
                        .map(|(ty, _)| self.llvm_type(ty, pos))
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                ),
                match proto.attrs.contains(Attributes::EXT) {
                    true => Some(Linkage::External),
                    false => None,
                },
            );
            self.current_ns
                .get()
                .funs
                .borrow_mut()
                .insert(proto.name.clone(), (fun, proto_clone));
            Ok(fun)
        } else {
            let fun = self.module.add_function(
                &qualified,
                self.llvm_type(&proto.ret, pos).fn_type(
                    proto
                        .args
                        .iter()
                        .map(|(ty, _)| self.llvm_type(ty, pos))
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                ),
                match proto.attrs.contains(Attributes::EXT) {
                    true => Some(Linkage::External),
                    false => None,
                },
            );
            self.current_ns
                .get()
                .funs
                .borrow_mut()
                .insert(proto.name.clone(), (fun, proto_clone));
            Ok(fun)
        }
    }

    /// Resolve unknown types to struct, union, or typedef'd types
    pub fn resolve_unknown(&self, ty: Type, pos: &Pos) -> Type {
        match ty {
            Type::Unknown(ref name)=> match (self.get_union(name), self.get_struct(name), self.get_typedef(name)) {
                (Some(_), Some(_), _) => panic!("Type {} can be both a union and a struct, prefix with struct or union keywords to remove abiguity", name),
                (Some(u), _, _) => Type::Union(u),
                (_, Some(s), _) => Type::Struct(s),
                (_, _, Some(ty)) => ty,
                (None, None, None) => {
                    error!("{}: Unknown union or struct type {}", pos, name);
                    panic!()
                },
            },
            Type::Ptr(ty) => self.resolve_unknown(*ty, pos).ptr_type(),
            Type::Array(ty, len) => Type::Array(Box::new(self.resolve_unknown(*ty, pos)), len),
            other => other
        }
    }

    /// Get all types and fill the struct bodies
    pub fn get_type_bodies(&self, ast: Vec<AstPos>) -> Vec<AstPos> {
        let mut ret = Vec::with_capacity(ast.len() / 2);
        for node in ast {
            match node.ast() {
                Ast::StructDec(Container {
                    name,
                    fields: Some(fields),
                    typeid,
                }) => {
                    let ty = self.struct_types.borrow()[*typeid];
                    ty.set_body(
                        fields
                            .iter()
                            .map(|(_, f)| self.llvm_type(f, &node.1))
                            .collect::<Vec<_>>()
                            .as_slice(),
                        true,
                    );

                    //Make sure no unknown types exist in struct body
                    let fields: Vec<(String, Type)> = fields
                        .iter()
                        .map(|(name, ty)| match ty {
                            Type::Unknown(_) => {
                                (name.clone(), self.resolve_unknown(ty.clone(), &node.1))
                            }
                            ty => (name.clone(), ty.clone()),
                        })
                        .collect();
                    trace!(
                        "Generating struct {}-{} body with fields {:?}",
                        name,
                        typeid,
                        fields
                    );

                    let mut types = self.current_ns.get().struct_types.borrow_mut();
                    types
                        .entry(name.to_string())
                        .and_modify(|c| c.fields = Some(fields.clone()));
                }
                Ast::StructDec(_) => (),
                Ast::UnionDec(Container {
                    name,
                    fields: Some(fields),
                    typeid,
                }) => {
                    let ty = self.struct_types.borrow()[*typeid];

                    //Make sure no unknown types exist in struct body
                    let fields: Vec<(String, Type)> = fields
                        .iter()
                        .map(|(name, ty)| match ty {
                            Type::Unknown(_) => {
                                (name.clone(), self.resolve_unknown(ty.clone(), &node.1))
                            }
                            ty => (name.clone(), ty.clone()),
                        })
                        .collect();
                    trace!(
                        "Generating code for union {}-{} body with fields {:?}",
                        name,
                        typeid,
                        fields
                    );

                    let largest = fields
                        .iter()
                        .max_by(|(_, prev), (_, this)| prev.size().cmp(&this.size()))
                        .expect("Union type with no fields!");
                    ty.set_body(&[self.llvm_type(&largest.1, &node.1)], false);
                }
                Ast::Ns(ns, stmts) => {
                    self.enter_ns(ns);
                    let stmts = self.get_type_bodies(stmts.clone());
                    self.exit_ns(ns.count());
                    ret.push(AstPos(Ast::Ns(ns.clone(), stmts), node.1))
                }
                _ => ret.push(node),
            }
        }
        ret
    }

    /// Generate code for all function prototypes
    fn scan_for_fns(&self, ast: Vec<AstPos>) -> Vec<AstPos> {
        let mut ret = Vec::with_capacity(ast.len());
        for node in ast {
            match node.ast() {
                Ast::FunProto(proto) => {
                    let mut proto = proto.clone();
                    proto.ret = self.resolve_unknown(proto.ret, &node.1);
                    for (arg, _) in proto.args.iter_mut() {
                        *arg = self.resolve_unknown(arg.clone(), &node.1);
                    }

                    self.gen_fun_proto(&proto, &node.1).unwrap();
                    trace!(
                        "Generated function prototype {}",
                        self.current_ns.get().qualify(&proto.name)
                    );
                }
                Ast::FunDef(proto, body) => {
                    let mut proto = proto.clone();
                    proto.ret = self.resolve_unknown(proto.ret, &node.1);
                    for (arg, _) in proto.args.iter_mut() {
                        *arg = self.resolve_unknown(arg.clone(), &node.1);
                    }

                    self.gen_fun_proto(&proto, &node.1).unwrap();
                    trace!(
                        "Generating function prototype for function definition {}",
                        self.current_ns.get().qualify(&proto.name)
                    );
                    ret.push(AstPos(Ast::FunDef(proto, body.clone()), node.1));
                }
                Ast::AsmFunDef(proto, asm, cons) => {
                    let mut proto = proto.clone();
                    proto.ret = self.resolve_unknown(proto.ret, &node.1);
                    for (arg, _) in proto.args.iter_mut() {
                        *arg = self.resolve_unknown(arg.clone(), &node.1);
                    }

                    self.gen_fun_proto(&proto, &node.1).unwrap();
                    trace!(
                        "Generating function prototype for assembly function definition {}",
                        self.current_ns.get().qualify(&proto.name)
                    );
                    ret.push(AstPos(
                        Ast::AsmFunDef(proto, asm.clone(), cons.clone()),
                        node.1,
                    ));
                }

                Ast::Ns(ns, stmts) => {
                    self.enter_ns(ns);
                    let stmts = self.scan_for_fns(stmts.clone());
                    self.exit_ns(ns.count());
                    ret.push(AstPos(Ast::Ns(ns.clone(), stmts), node.1))
                }
                _ => ret.push(node),
            }
        }
        ret
    }

    /// Get all `using` directives after finding all namespaces
    fn get_using(&self, ast: Vec<AstPos>) -> Vec<AstPos> {
        let mut ret = Vec::with_capacity(ast.len());
        for node in ast {
            match node.ast() {
                Ast::Using(name) => {
                    let ns = match self.root.get_ns(name.clone()) {
                        Some(ns) => ns,
                        None => {
                            warn!("{}: Imported unknown namespace {}", &node.1, name);
                            continue;
                        }
                    };
                    self.current_ns
                        .get()
                        .nested
                        .borrow_mut()
                        .insert(ns.name.clone(), ns);
                    trace!(
                        "Imported namespace {} into namespace {}",
                        name,
                        self.current_ns.get().full_path()
                    )
                }
                //Insert a user-defined typedef
                Ast::TypeDef(name, ty) => {
                    let ty = self.resolve_unknown(ty.clone(), &node.1);
                    self.current_ns
                        .get()
                        .typedefs
                        .borrow_mut()
                        .insert(name.clone(), ty);
                    trace!("Generated typedef {}", name);
                }
                Ast::Ns(ns, stmts) => {
                    self.enter_ns(ns);
                    let stmts = self.get_using(stmts.clone());
                    self.exit_ns(ns.count());
                    ret.push(AstPos(Ast::Ns(ns.clone(), stmts), node.1))
                }
                _ => ret.push(node),
            }
        }
        ret
    }

    /// Get all constant values
    pub fn get_consts(&mut self, ast: Vec<AstPos>) -> Vec<AstPos> {
        let mut ret = Vec::with_capacity(ast.len());
        for node in ast {
            match node.ast() {
                Ast::GlobalDef(ty, name, val, attrs) => {
                    let ty = self.resolve_unknown(ty.clone(), &node.1);

                    let qname = self.current_ns.get().qualify(&name).to_string();
                    match ty {
                        Type::Struct(_) | Type::Union(_) => {
                            panic!(
                                "{}: Struct and union types cannot be used as global values!",
                                node.1
                            );
                        }
                        _ => (),
                    }

                    trace!(
                        "{}: Adding constant value {} with type {} to {}",
                        node.1,
                        qname,
                        ty,
                        self.current_ns.get().full_path()
                    );
                    let global = self.module.add_global(
                        self.llvm_type(&ty, &node.1),
                        Some(AddressSpace::Global),
                        qname.as_str(),
                    );

                    if attrs.contains(Attributes::CONST) {
                        global.set_constant(true);
                    }
                    if attrs.contains(Attributes::EXT) {
                        global.set_linkage(Linkage::External);
                    }

                    if let Some(val) = val {
                        global.set_initializer(&match self.gen(val, false) {
                            Some(val) => BasicValueEnum::try_from(val).unwrap(),
                            None => {
                                panic!(
                                    "{}: Failed to generate code for global variable {}",
                                    node.1,
                                    self.current_ns.get().qualify(&name)
                                );
                            }
                        });
                    }
                    self.current_ns
                        .get()
                        .consts
                        .borrow_mut()
                        .insert(name.clone(), (global, ty.clone()));
                }
                Ast::Ns(ns, stmts) => {
                    self.enter_ns(ns);
                    let stmts = self.get_consts(stmts.clone());
                    self.exit_ns(ns.count());
                    ret.push(AstPos(Ast::Ns(ns.clone(), stmts), node.1))
                }
                _ => ret.push(node),
            }
        }
        ret
    }

    /// Walk the AST and get any declared types or functions
    pub fn scan_decls(&self, ast: Vec<AstPos>) -> Vec<AstPos> {
        let ast = self.get_opaques(ast);
        let ast = self.get_using(ast);
        let ast = self.get_type_bodies(ast);
        self.scan_for_fns(ast)
    }

    /// Convert the AST types to LLVM types
    pub fn llvm_type(&self, ty: &Type, pos: &Pos) -> BasicTypeEnum<'c> {
        match ty {
            Type::Integer{
                width, 
                signed: _
            } => self.ctx.custom_width_int_type(*width as u32).as_basic_type_enum(),
            Type::Ptr(internal) => self.llvm_type(internal, pos).ptr_type(inkwell::AddressSpace::Generic).as_basic_type_enum(),
            Type::Union(u) => self.struct_types.borrow()[u.typeid].as_basic_type_enum(),
            Type::Struct(u) => self.struct_types.borrow()[u.typeid].as_basic_type_enum(),
            Type::Unknown(name) => match (self.get_union(name), self.get_struct(name), self.get_typedef(name)) {
                (Some(_), Some(_), _) => panic!("{}: Type {} can be both a union and a struct, prefix with struct or union keywords to remove abiguity", pos, name),
                (Some(u), _, _) => self.struct_types.borrow()[u.typeid].as_basic_type_enum(),
                (_, Some(s), _) => self.struct_types.borrow()[s.typeid].as_basic_type_enum(),
                (_, _, Some(ty)) => self.llvm_type(&ty, pos),
                (None, None, None) => {
                    error!("{}: Unknown union or struct type {}", pos, name);
                    panic!()
                },
            },
            Type::Void => panic!("{}: Cannot create void type in LLVM!", pos),
            Type::Array(ty, len) => self.llvm_type(ty, pos).array_type(*len as u32).as_basic_type_enum()
        }
    }
}
