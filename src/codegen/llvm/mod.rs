//! Generating LLVM IR from a parsed and type lowered AST

use hashbrown::HashMap;
use inkwell::{builder::Builder, context::Context, module::Module, types::AnyTypeEnum};

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
}

impl<'ctx> LlvmCodeGenerator<'ctx> {
    /// Create a new code generator from an LLVM context
    pub fn new(spark: SparkCtx, ctx: &'ctx Context) -> Self {
        Self {
            builder: ctx.create_builder(),
            ctx,
            spark,
            llvm_types: HashMap::new(),
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
                SparkDef::TypeDef(id) => {
                    let llvm_ty = match &self.spark[*id].data {
                        TypeData::Integer { signed: _, width } => match width {
                            IntegerWidth::Eight => self.ctx.i8_type().into(),
                            IntegerWidth::Sixteen => self.ctx.i16_type().into(),
                            IntegerWidth::ThirtyTwo => self.ctx.i32_type().into(),
                            IntegerWidth::SixtyFour => self.ctx.i64_type().into(),
                        },
                        TypeData::
                    };
                },
                _ => continue,
            }
        }
    }
}


