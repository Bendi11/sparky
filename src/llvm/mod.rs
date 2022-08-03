use std::convert::TryFrom;

use inkwell::{module::Module, context::Context, builder::Builder, values::FunctionValue, types::{AnyTypeEnum, BasicTypeEnum, BasicType}, AddressSpace};

use crate::{util::files::Files, ir::{IrContext, TypeId, types::{IrType, IrFloatType}}, arena::Arena, ast::IntegerWidth};


/// Structure containing all state needed to generate LLVM IR from spark IR
pub struct LLVMCodeGenerator<'files, 'ctx, 'llvm> {
    files: &'files Files,
    irctx: &'ctx mut IrContext,
    ctx: &'llvm Context,
    root: Module<'llvm>,
    build: Builder<'llvm>,
    llvm_funs: Arena<FunctionValue<'llvm>>,
    llvm_types: Arena<AnyTypeEnum<'llvm>>,
}

impl<'files, 'ctx, 'llvm> LLVMCodeGenerator<'files, 'ctx, 'llvm> {
    /// Create a new [LLVMCodeGenerator] from shared reference to a [Files] structure and unique
    /// reference to the IR context
    pub fn new(files: &'files Files, irctx: &'ctx mut IrContext, ctx: &'llvm Context) -> Self {
        let root = ctx.create_module("spark_module");
        Self {
            llvm_funs: irctx
                .funs
                .secondary(|fun| root.add_function(fun.name.as_str(), , linkage))
            files,
            irctx,
            ctx,
            root,
            build: ctx.create_builder(),
        }
    }
    
    /// Generate LLVM IR for a single IR type
    pub fn gen_type(&self, ty: TypeId) -> BasicTypeEnum<'llvm> {
        match &self.irctx[self.irctx.unwrap_alias(ty)] {
            IrType::Integer(ity) => match ity.width {
                IntegerWidth::Eight => self.ctx.i8_type().into(),
                IntegerWidth::Sixteen => self.ctx.i16_type().into(),
                IntegerWidth::ThirtyTwo => self.ctx.i32_type().into(),
                IntegerWidth::SixtyFour => self.ctx.i64_type().into(),
            },
            IrType::Float(IrFloatType { doublewide }) => match doublewide {
                true => self.ctx.f64_type().into(),
                false => self.ctx.f32_type().into(),
            },
            IrType::Bool => self.ctx.bool_type().into(),
            IrType::Unit => self.ctx.i8_type().into(),
            IrType::Ptr(ty) => self.gen_type(*ty).ptr_type(AddressSpace::Generic).into(),
            IrType::Fun(f) => {
                let return_ty = self.gen_type(f.return_ty);
                let params = f
                    .params
                    .iter()
                    .map(|(ty, _)| self.gen_type(*ty).into())
                    .collect::<Vec<_>>();

                return_ty.fn_type(&params, false).into()
            },
            IrType::Alias { .. } => unreachable!(),
        }
    } 
}
