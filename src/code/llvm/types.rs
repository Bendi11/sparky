//! The `types` module provides implementations for the [Compiler] struct for finding types, functions, and converting AST types to 
//! LLVM types
use crate::ast::AstPos;

use super::*;

pub enum Either<A, B> {
    This(A),
    That(B)
}

impl<'c> Compiler<'c> {
    /// Get a struct type from the given path
    pub fn get_struct(&self, name: impl AsRef<str>) -> Option<(StructType<'c>, Container)> {
        let name = name.as_ref();
        self.struct_types.get(name).cloned()
    }

    /// Get a union type from the given path
    pub fn get_union(&self, name: impl AsRef<str>) -> Option<(StructType<'c>, Container)> {
        let name = name.as_ref();
        self.union_types.get(name).cloned()
    }

    /// Get a typedef'd type from the given path
    pub fn get_typedef(&self, name: impl AsRef<str>) -> Option<Type> {
        let name = name.as_ref();
        self.typedefs.get(name).cloned()
    }

    /// Get a struct type from the given path
    pub fn get_fun(&self, name: impl AsRef<str>) -> Option<(FunctionValue<'c>, FunProto)> {
        let name = name.as_ref();
        self.funs.get(name).cloned()
    }

    /// Get all type definitions and track them as opaque struct types
    fn get_opaques<'a>(&mut self, ast: Vec<AstPos>) -> Vec<AstPos> {
        ast.iter().for_each(|node| {
            match node.ast() {
                Ast::StructDec(container) | Ast::UnionDec(container) => {
                    trace!("Generating initial opaque llvm type for struct/union type {}", container.name);
                    let ty = self.ctx.opaque_struct_type(&container.name);
                    self.struct_types.insert(container.name.clone(), (ty, container.clone()));
                },
                _ => ()
            }
        });
        ast
    }


    /// Generate code for a function prototype
    pub(super) fn gen_fun_proto(&mut self, proto: &FunProto) -> Result<FunctionValue<'c>, String> {
        if self.module.get_function(proto.name.as_str()).is_some() {
            return Err(format!("Function {} defined twice", proto.name));
        }

        if proto.ret == Type::Void {
            let proto_clone = proto.clone();
            let fun = self.module.add_function(
                proto.name.as_str(),
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
            self.funs.insert(proto.name.clone(), (fun, proto_clone));
            Ok(fun)
        } else {
            let proto_clone = proto.clone();
            let fun = self.module.add_function(
                proto.name.as_str(),
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
            self.funs.insert(proto.name.clone(), (fun, proto_clone));
            Ok(fun)
        }
    }
    
    /// Get all types and fill the struct bodies
    pub fn get_type_bodies(&mut self, ast: Vec<AstPos>) -> Vec<AstPos> {
        ast.into_iter().filter(|node| match node.ast() {
            Ast::StructDec(Container{name, fields: Some(fields)}) => {
                let ty = self.module.get_struct_type(&name).unwrap();
                ty.set_body(fields.iter().map(|(_, f)| self.llvm_type(f)).collect::<Vec<_>>().as_slice(), true);
                let (_, col) = self.struct_types.get_mut(name).unwrap();
                trace!("Generating struct {} body with fields {:?}", name, fields);
                col.fields = Some(fields.clone());
                false
            },
            Ast::StructDec(_) => false,
            Ast::UnionDec(con) => {
                let ty = self.module.get_struct_type(&con.name).unwrap();
                let largest = con.fields.as_ref().unwrap().iter().max_by(|(_, prev), (_, this)| prev.size().cmp(&this.size())).expect("Union type with no fields!");
                ty.set_body(&[self.llvm_type(&largest.1)], false);
                false
            },
            _ => true
        }).collect()
    }
    
    /// Generate code for all function prototypes 
    fn scan_for_fns(&mut self, ast: Vec<AstPos>) -> Vec<AstPos> {
        ast.into_iter().filter(|node| match node.ast() {
            Ast::FunProto(proto)  => {
                self.gen_fun_proto(proto).unwrap();
                trace!("Generated function prototype {}", proto.name);
                false
            },
            Ast::FunDef(proto, _) => {
                self.gen_fun_proto(proto).unwrap();
                trace!("Generation function prototype for function definition {}", proto.name);
                true
            },
            //Insert a user-defined typedef
            Ast::TypeDef(name, ty) => {
                self.typedefs.insert(name.clone(), ty.clone());
                false
            },
            _ => true
        }).collect()
    }

    /// Walk the AST and get any declared types or functions
    pub fn scan_decls(&mut self, ast: Vec<AstPos>) -> Vec<AstPos> {
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