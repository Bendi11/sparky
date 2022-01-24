//! Generating LLVM IR from a parsed and type lowered AST

use std::convert::TryFrom;

use hashbrown::HashMap;
use inkwell::{AddressSpace, builder::Builder, context::Context, module::Module, targets::{TargetData, TargetMachine}, types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum}};

use super::{
    ir::{ModId, SparkCtx, TypeId, SparkDef, TypeData},
    ast::IntegerWidth,
};

/// Structure that generates LLVM IR modules from a parsed and 
/// type lowered AST module
pub struct LlvmCodeGenerator<'ctx> {
    pub ctx: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub spark: SparkCtx,
    llvm_types: HashMap<TypeId, AnyTypeEnum<'ctx>>,
    target: TargetData,
}

impl<'ctx> LlvmCodeGenerator<'ctx> {
    /// Create a new code generator from an LLVM context
    pub fn new(spark: SparkCtx, ctx: &'ctx Context) -> Self {
        Self {
            builder: ctx.create_builder(),
            ctx,
            spark,
            llvm_types: HashMap::new(),
            target: TargetData::create(TargetMachine::get_default_triple().as_str())
        }
    }
    
    /// Lower a type-lowered module to LLVM IR 
    pub fn codegen_module(&mut self, module: ModId) -> Module<'ctx> {
        let mut llvm_mod = self.ctx.create_module(self.spark[module].name.as_str());
    }
    
    /// Generate forward type declarations for a module
    fn forward_types(&mut self, llvm: ModId, module: ModId) {
        for def in self.spark[module].defs.iter().map(|(_, v)| v) {
            match def {
                SparkDef::TypeDef(id) => match &self.spark[*id].data {
    
                },
                _ => continue,
            }
        }
    }
    
    /// Create an LLVM type from a type ID
    fn llvm_ty(&mut self, id: TypeId) -> AnyTypeEnum<'ctx> {
        if let Some(ty) = self.llvm_types.get(&id) {
            return *ty;
        }

        match &self.spark[id].data {
            TypeData::Integer { signed: _, width } => match width {
                IntegerWidth::Eight => self.ctx.i8_type().into(),
                IntegerWidth::Sixteen => self.ctx.i16_type().into(),
                IntegerWidth::ThirtyTwo => self.ctx.i32_type().into(),
                IntegerWidth::SixtyFour => self.ctx.i64_type().into(),
            },
            TypeData::Bool => self.ctx.bool_type().into(),
            TypeData::Tuple(elems) => {
                let elems = elems
                    .iter()
                    .map(|id| BasicTypeEnum::try_from(self.llvm_ty(*id)).unwrap())
                    .collect::<Vec<_>>();
                self.ctx.struct_type(&elems, false).into()
            },
            TypeData::Struct { fields } => {
                let fields = fields
                    .iter()
                    .map(|(id, _)| BasicTypeEnum::try_from(self.llvm_ty(*id)).unwrap())
                    .collect::<Vec<_>>();
                self.ctx.struct_type(&fields, false).into()
            },
            TypeData::Alias(id) => self.llvm_ty(*id),
            TypeData::Pointer(id) => 
                BasicTypeEnum::try_from(self.llvm_ty(*id))
                    .unwrap()
                    .ptr_type(AddressSpace::Generic)
                    .into(),
            TypeData::Array { element, len } => 
                BasicTypeEnum::try_from(self.llvm_ty(*element))
                    .unwrap()
                    .array_type(*len as u32)
                    .into(),
            TypeData::Unit => self.ctx.void_type().into(),
            TypeData::Invalid => unreachable!(),
            TypeData::Float { doublewide } => match doublewide {
                true => self.ctx.f64_type().into(),
                false => self.ctx.f32_type().into(),
            },
            TypeData::Function(ty) => {
                let return_ty = self.llvm_ty(ty.return_ty);
                let args = ty
                    .args
                    .iter()
                    .map(|ty| BasicTypeEnum::try_from(self.llvm_ty(*ty)).unwrap().into())
                    .collect::<Vec<_>>();
                match return_ty {
                    AnyTypeEnum::VoidType(return_ty) => 
                        return_ty.fn_type(&args, false),
                    _ => BasicTypeEnum::try_from(return_ty)
                        .unwrap()
                        .fn_type(&args, false)
                }.into()
            },
            TypeData::Enum { parts } => {
                let parts = parts
                    .iter()
                    .map(|ty| self.llvm_ty(*ty))
                    .collect::<Vec<_>>();
                let max_size = parts.iter().max_by_key(|ty| {
                    BasicTypeEnum::try_from(ty)
                })
            }
        }

    }
}


