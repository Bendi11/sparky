
use inkwell::{module::{Module, Linkage}, context::Context, builder::Builder, values::FunctionValue, types::{AnyTypeEnum, BasicTypeEnum, BasicType, FunctionType}, AddressSpace};

use crate::{util::files::Files, ir::{IrContext, TypeId, types::{IrType, IrFloatType, FunType}}, arena::Arena, ast::IntegerWidth};


/// Structure containing all state needed to generate LLVM IR from spark IR
pub struct LLVMCodeGenerator<'files, 'ctx, 'llvm> {
    files: &'files Files,
    irctx: &'ctx mut IrContext,
    ctx: &'llvm Context,
    root: Module<'llvm>,
    build: Builder<'llvm>,
    llvm_funs: Arena<FunctionValue<'llvm>>,
    llvm_types: Arena<BasicTypeEnum<'llvm>>,
}

impl<'files, 'ctx, 'llvm> LLVMCodeGenerator<'files, 'ctx, 'llvm> {
    /// Create a new [LLVMCodeGenerator] from shared reference to a [Files] structure and unique
    /// reference to the IR context
    pub fn new(files: &'files Files, irctx: &'ctx mut IrContext, ctx: &'llvm Context) -> Self {
        let root = ctx.create_module("spark_module");
        Self {
            llvm_funs: irctx
                .funs
                .secondary(|fun| root.add_function(fun.name.as_str(), Self::gen_funtype(ctx, irctx, &fun.ty), Some(Linkage::External))),
            llvm_types: irctx
                .types
                .secondary(|ty| Self::gen_type(ctx, irctx, ty)),
            files,
            irctx,
            ctx,
            root,
            build: ctx.create_builder(),
        }
    }
    
    /// Generate LLVM IR for a single IR type
    pub fn gen_type(ctx: &'llvm Context, irctx: &'ctx IrContext, ty: &IrType) -> BasicTypeEnum<'llvm> {
        match ty {
            IrType::Integer(ity) => match ity.width {
                IntegerWidth::Eight => ctx.i8_type().into(),
                IntegerWidth::Sixteen => ctx.i16_type().into(),
                IntegerWidth::ThirtyTwo => ctx.i32_type().into(),
                IntegerWidth::SixtyFour => ctx.i64_type().into(),
            },
            IrType::Float(IrFloatType { doublewide }) => match doublewide {
                true => ctx.f64_type().into(),
                false => ctx.f32_type().into(),
            },
            IrType::Bool => ctx.bool_type().into(),
            IrType::Unit => ctx.i8_type().into(),
            IrType::Ptr(ty) => Self::gen_type(ctx, irctx, &irctx[*ty]).ptr_type(AddressSpace::Generic).into(),
            IrType::Fun(f) => Self::gen_funtype(ctx, irctx, f)
                    .ptr_type(AddressSpace::Generic)
                    .into(),
            IrType::Struct(s_ty) => {
                let fields = s_ty
                    .fields
                    .iter()
                    .map(|field| Self::gen_type(ctx, irctx, &irctx[field.ty]))
                    .collect::<Vec<_>>();
                ctx.struct_type(&fields, false).into()
            },
            IrType::Sum(variants) => {
                let variants = variants
                    .iter()
                    .map(|variant| Self::gen_type(ctx, irctx, &irctx[*variant]))
                    .collect::<Vec<_>>();

                let largest_size = variants
                    .iter()
                    .map(|ty| ty.size_of().unwrap().get_zero_extended_constant().unwrap())
                    .max()
                    .unwrap();

                ctx
                    .struct_type(&[ctx.i8_type().into(), ctx.i8_type().array_type(largest_size as u32).into()], false)
                    .into()
            },
            IrType::Array(ty, sz) => Self::gen_type(ctx, irctx, &irctx[*ty]).array_type(*sz as u32).into(),
            IrType::Alias { .. } | IrType::Invalid => unreachable!(),
        }
    }
    
    /// Generate the LLVM IR signature for the given IR function signature
    fn gen_funtype(ctx: &'llvm Context, irctx: &'ctx IrContext, ty: &FunType) -> FunctionType<'llvm> {
        let return_ty = Self::gen_type(ctx, irctx, &irctx[ty.return_ty]);
        let params = ty
            .params
            .iter()
            .map(|(ty, _)| Self::gen_type(ctx, irctx, &irctx[*ty]).into())
            .collect::<Vec<_>>();

        return_ty
            .fn_type(&params, false)
    }
}
