//! The `types` module provides implementations for the [Compiler] struct for finding types, functions, and converting AST types to
//! LLVM types

use crate::{ast::AstPos, code::ns::Path};

use super::*;

pub enum Either<A, B> {
    This(A),
    That(B),
}

impl<'a, 'c> Compiler<'a, 'c> {
    /// Get a struct type from the given path
    #[inline]
    pub fn get_struct(&self, name: impl AsRef<str>) -> Option<(StructType<'c>, Container)> {
        self.current_ns.get().get_struct(name.as_ref().parse().unwrap())
    }

    /// Get a union type from the given path
    pub fn get_union(&self, name: impl AsRef<str>) -> Option<(StructType<'c>, Container)> {
        self.current_ns.get().get_union(name.as_ref().parse().unwrap())
    }

    /// Get a typedef'd type from the given path
    pub fn get_typedef(&self, name: impl AsRef<str>) -> Option<Type> {
        self.current_ns.get().get_typedef(name.as_ref().parse().unwrap())
    }

    /// Get a struct type from the given path
    pub fn get_fun(&self, name: impl AsRef<str>) -> Option<(FunctionValue<'c>, FunProto)> {
        self.current_ns.get().get_fun(name.as_ref().parse().unwrap())
    }

    /// Enter a new namespace or create one if the namespace doesn't exist
    fn enter_ns(&self, ns: &Path) {
        let mut iter = ns.parts();
        loop {
            match iter.next() {
                Some(name) => {
                    match self.current_ns.get().nested.borrow().contains_key(name) {
                        true => self.current_ns.set(self.current_ns.get().get_ns(name.parse().unwrap()).unwrap()),
                        false => {
                            let ns = self.arena.alloc(Ns::new_empty(name.clone()));
                            self.current_ns.get().add_ns(ns);
                            self.current_ns.set(ns);
                        }
                    }
                },
                None => break
            }
        }
    }

    /// Exit a certain amount of namepsaces by going one namespace up
    fn exit_ns(&self, depth: usize) {
        for _ in 0..depth {
            self.current_ns.set(self.current_ns.get().parent.borrow().as_ref().unwrap());
        }
    } 

    /// Get all type definitions and track them as opaque struct types
    fn get_opaques(&self, ast: Vec<AstPos>) -> Vec<AstPos> {
        
        for node in ast.iter() {
            match node.ast() {
                Ast::StructDec(container) | Ast::UnionDec(container) => {
                    trace!(
                        "Generating initial opaque llvm type for struct/union type {}",
                        container.name
                    );
                    let name = self.current_ns.get().qualify(&container.name).to_string();
                    let ty = self.ctx.opaque_struct_type(&name);
                    self.current_ns.get().struct_types.borrow_mut()
                        .insert(container.name.clone(), (ty, container.clone()));
                },
                Ast::Ns(ns, stmts) => {
                    self.enter_ns(&ns);
                    self.get_opaques(stmts.clone());
                    self.exit_ns(ns.count());
                }
                _ => (),
            }
        }
        ast
    }

    /// Generate code for a function prototype
    pub(super) fn gen_fun_proto(&self, proto: &FunProto) -> Result<FunctionValue<'c>, String> {
        let qualified = self.current_ns.get().qualify(&proto.name).to_string();
        if self.module.get_function(qualified.as_str()).is_some() {
            return Err(format!("Function {} defined twice", qualified));
        }

        if proto.ret == Type::Void {
            let proto_clone = proto.clone();
            let fun = self.module.add_function(
                self.current_ns.get().qualify(proto.name.as_str()).to_string().as_str(),
                self.ctx.void_type().fn_type(
                    proto
                        .args
                        .iter()
                        .map(|(ty, _)| self.llvm_type(ty))
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                ),
                None,
            );
            self.current_ns.get().funs.borrow_mut().insert(proto.name.clone(), (fun, proto_clone));
            Ok(fun)
        } else {
            let proto_clone = proto.clone();
            let fun = self.module.add_function(
                self.current_ns.get().qualify(&proto.name).to_string().as_str(),
                self.llvm_type(&proto.ret).fn_type(
                    proto
                        .args
                        .iter()
                        .map(|(ty, _)| self.llvm_type(ty))
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                ),
                None,
            );
            self.current_ns.get().funs.borrow_mut().insert(proto.name.clone(), (fun, proto_clone));
            Ok(fun)
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
                    }) => {
                        let ty = self.module.get_struct_type(self.current_ns.get().qualify(name).to_string().as_str()).unwrap();
                        ty.set_body(
                            fields
                                .iter()
                                .map(|(_, f)| self.llvm_type(f))
                                .collect::<Vec<_>>()
                                .as_slice(),
                            true,
                        );
                        let mut types = self.current_ns.get().struct_types.borrow_mut();
                        let (_, col) = types.get_mut(name).unwrap();
                        trace!("Generating struct {} body with fields {:?}", name, fields);
                        col.fields = Some(fields.clone());
                        
                    }
                    Ast::StructDec(_) => (),
                    Ast::UnionDec(con) => {
                        let ty = self.module.get_struct_type(self.current_ns.get().qualify(&con.name).to_string().as_str()).unwrap();
                        let largest = con
                            .fields
                            .as_ref()
                            .unwrap()
                            .iter()
                            .max_by(|(_, prev), (_, this)| prev.size().cmp(&this.size()))
                            .expect("Union type with no fields!");
                        ty.set_body(&[self.llvm_type(&largest.1)], false);
                        
                    },
                    Ast::Ns(ns, stmts) => {
                        self.enter_ns(&ns);
                        let stmts = self.get_type_bodies(stmts.clone());
                        self.exit_ns(ns.count());
                        ret.push(AstPos(Ast::Ns(ns.clone(), stmts), node.1))
                    },
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
                    self.gen_fun_proto(&proto).unwrap();
                    trace!("Generated function prototype {}", proto.name);
                    
                }
                Ast::FunDef(proto, _) => {
                    self.gen_fun_proto(&proto).unwrap();
                    trace!(
                        "Generation function prototype for function definition {}",
                        &proto.name
                    );
                    ret.push(node);
                }
                //Insert a user-defined typedef
                Ast::TypeDef(name, ty) => {
                    self.current_ns.get().typedefs.borrow_mut().insert(name.clone(), ty.clone());
                    trace!("Generated typedef {}", name);
                },
                Ast::Ns(ns, stmts) => {
                    self.enter_ns(&ns);
                    let stmts = self.scan_for_fns(stmts.clone());
                    self.exit_ns(ns.count());
                    ret.push(AstPos(Ast::Ns(ns.clone(), stmts), node.1))
                },
                _ => ret.push(node),
            }
        }
        ret
    }

    /// Walk the AST and get any declared types or functions
    pub fn scan_decls(&self, ast: Vec<AstPos>) -> Vec<AstPos> {
        let ast = self.get_opaques(ast);
        let ast = self.get_type_bodies(ast);
        self.scan_for_fns(ast)
    }

    /// Convert the AST types to LLVM types
    pub fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'c> {
        match ty {
            Type::Integer{
                width, 
                signed: _
            } => self.ctx.custom_width_int_type(*width as u32).as_basic_type_enum(),
            Type::Ptr(internal) => self.llvm_type(internal).ptr_type(inkwell::AddressSpace::Generic).as_basic_type_enum(),
            Type::Union(u) => self.get_union(&u.name).unwrap_or_else(|| panic!("Failed to get unknown union type {}", u.name)).0.as_basic_type_enum(),
            Type::Struct(u) => self.get_struct(&u.name).unwrap_or_else(|| panic!("Failed to get unknown struct type {}", u.name)).0.as_basic_type_enum(),
            Type::Unknown(name) => match (self.get_union(name), self.get_struct(name), self.get_typedef(name)) {
                (Some(_), Some(_), _) => panic!("Type {} can be both a union and a struct, prefix with struct or union keywords to remove abiguity", name),
                (Some(u), _, _) => u.0.as_basic_type_enum(),
                (_, Some(s), _) => s.0.as_basic_type_enum(),
                (_, _, Some(ty)) => self.llvm_type(&ty),
                (None, None, None) => panic!("Unknown union or struct type {}", name),
            },
            Type::Void => panic!("Cannot create void type in LLVM!"),
        }
    }
}
