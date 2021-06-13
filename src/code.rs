use std::convert::TryFrom;

use hashbrown::HashMap;
use inkwell::{builder::Builder, context::Context, module::{Module}, types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, StructType}, values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue}};
use crate::{Type, ast::{Ast, FunProto}, types::Container};


/// The `Compiler` struct is used to generate an executable with LLVM from the parsed AST.
pub struct Compiler<'c> {
    /// The LLVM context 
    ctx: &'c Context,

    /// A hash map of identifiers to defined struct types
    struct_types: HashMap<String, StructType<'c>>,

    /// A hash map of identifiers to defined union types
    union_types: HashMap<String, StructType<'c>>,

    /// The LLVM module that we will be writing code to
    module: Module<'c>,

    /// The IR builder that we use to build LLVM IR
    build: Builder<'c>,

    /// The function that we are currently generating code in
    current_fn: Option<FunctionValue<'c>>,

    /// The Abstract syntax tree 
    ast: Vec<Ast>,
}

impl<'c> Compiler<'c> {
    /// Create a new `Compiler` from an LLVM context struct
    pub fn new(ctx: &'c Context, ast: Vec<Ast>) -> Self {
        Self {
            ctx,
            struct_types: HashMap::new(),
            union_types: HashMap::new(),
            build: ctx.create_builder(),
            module: ctx.create_module("spark_llvm_module"),
            current_fn: None,
            ast,
        }
    }

    /// Convert the AST types to LLVM types
    pub fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'c> {
        match ty {
            Type::Integer{
                width, 
                signed: _
            } => self.ctx.custom_width_int_type(*width as u32).as_basic_type_enum(),
            Type::Ptr(internal) => self.llvm_type(internal).ptr_type(inkwell::AddressSpace::Generic).as_basic_type_enum(),
            Type::Struct(con) => {
                self.ctx.struct_type(con.fields.iter().map(|(_, f)| self.llvm_type(f)).collect::<Vec<_>>().as_slice(), false).as_basic_type_enum()
            },
            Type::Union(con) => {
                let largest = con.fields.iter().max_by(|(_, prev), (_, this)| prev.size().cmp(&this.size())).expect("Union type with no fields!");
                self.ctx.struct_type(&[self.llvm_type(largest.1)], false).as_basic_type_enum()
            },
            Type::UnknownStruct(name) => match self.struct_types.get(name) {
                Some(s_ty) => s_ty.as_basic_type_enum(),
                None => panic!("Unknown struct type {}", name),
            },
            Type::UnknownUnion(name) => match self.union_types.get(name) {
                Some(u_ty) => u_ty.as_basic_type_enum(),
                None => panic!("Unknown union type {}", name),
            },
            Type::Unknown(name) => match (self.union_types.get(name), self.struct_types.get(name)) {
                (Some(_), Some(_)) => panic!("Type {} can be both a union and a struct, prefix with struct or union keywords to remove abiguity", name),
                (Some(u), _) => u.as_basic_type_enum(),
                (_, Some(s)) => s.as_basic_type_enum(),
                (None, None) => panic!("Unknown union or struct type {}", name),
            },
            Type::Void => panic!("Cannot create void type in LLVM!"),
        }
    }

    /// Generate code for a function prototype
    pub fn gen_fun_proto(&mut self, proto: FunProto) -> Result<FunctionValue<'c>, String> {
        if self.module.get_function(proto.name.as_str()).is_some() {
            return Err(format!("Function {} defined twice", proto.name))
        }
        Ok(self.module.add_function(
            proto.name.as_str(), 
            self.llvm_type(&proto.ret).fn_type(proto.args.iter().map(|(ty, _)| self.llvm_type(ty)).collect::<Vec<_>>().as_slice(), false), 
            None
        ))
    }

    /// Generate code for one expression
    pub fn gen(&mut self, node: &Ast) -> AnyValueEnum<'c> {
        match node {
            Ast::NumLiteral(ty, num) => {
                self.llvm_type(ty).into_int_type().const_int_from_string(num.as_str(), inkwell::types::StringRadix::Decimal).unwrap().as_any_value_enum()
            },
            Ast::Ret(node) => {
                let ret = self.gen(node);
                self.build.build_return(Some(&BasicValueEnum::try_from(ret).unwrap())).as_any_value_enum()
            },
            Ast::FunCall(name, args) => match self.module.get_function(name.as_str()) {
                Some(f) => {
                    let args = args.iter().map(|n| BasicValueEnum::try_from(self.gen(n)).expect("Failed to convert any value enum to basic value enum when calling function")).collect::<Vec<_>>();
                    self.build.build_call(f, args.as_ref(), "tmp_fncall").as_any_value_enum()
                }
                None => panic!("Calling unknown function {}", name),
            },
            Ast::FunDef(proto, body) => {
                let f = match self.module.get_function(proto.name.as_str()) {
                    Some(f) => f,
                    None => self.gen_fun_proto(proto.clone()),
                };
                let bb
                
            }
            _ => unimplemented!("Nope"),
        }
    }

    /// Get all struct and union type declarations from the top level of the AST and remove them
    fn get_type_decls(&mut self) {
        let new_ast = self.ast.clone().into_iter().filter_map(|node| {
            match node {
                Ast::StructDef(c) => {
                    self.struct_types.insert(c.name.clone(), self.llvm_type(&Type::Struct(c.clone())).into_struct_type() );
                    None
                }
                Ast::UnionDef(c) => {
                    self.union_types.insert(c.name.clone(), self.llvm_type(&Type::Union(c.clone())).into_struct_type() );
                    None
                }
                other => Some(other),
            }
        }).collect::<Vec<_>>();
        self.ast = new_ast;
    }

    /// Filter the AST, adding function prototypes to the module ahead of time
    fn get_fn_protos(&mut self) {
        let new_ast = self.ast.clone().into_iter().filter_map(|node| {
            match node {
                Ast::FunProto(p) | Ast::FunDef(p, _) => {
                    self.gen_fun_proto(p).unwrap();
                    None
                }
                other => Some(other),
            }
        }).collect::<Vec<_>>();
        self.ast = new_ast;
    }

}