
use hashbrown::HashMap;
use inkwell::{module::{Module, Linkage}, context::Context, builder::Builder, values::{FunctionValue, PointerValue}, types::{AnyTypeEnum, BasicTypeEnum, BasicType, FunctionType, IntType}, AddressSpace, passes::PassManager, targets::{Target, InitializationConfig, RelocMode, CodeModel, TargetMachine, FileType}, OptimizationLevel, basic_block::BasicBlock};

use crate::{util::files::Files, ir::{IrContext, TypeId, types::{IrType, IrFloatType, FunType, IrIntegerType}, FunId, BBId}, arena::Arena, ast::IntegerWidth, CompileOpts};

pub mod stmt;
pub mod expr;

/// Structure containing all state needed to generate LLVM IR from spark IR
pub struct LLVMCodeGenerator<'files, 'ctx, 'llvm> {
    state: LLVMCodeGeneratorState<'files, 'llvm>,
    irctx: &'ctx mut IrContext,
}

pub struct LLVMCodeGeneratorState<'files, 'llvm> {
    files: &'files Files,
    ctx: &'llvm Context,
    root: Module<'llvm>,
    build: Builder<'llvm>,
    llvm_funs: Arena<FunctionValue<'llvm>>,
    llvm_types: Arena<BasicTypeEnum<'llvm>>,
    llvm_vars: Arena<Option<PointerValue<'llvm>>>,
    llvm_bbs: HashMap<BBId, BasicBlock<'llvm>>,

}

impl<'files, 'ctx, 'llvm> LLVMCodeGenerator<'files, 'ctx, 'llvm> {
    /// Create a new [LLVMCodeGenerator] from shared reference to a [Files] structure and unique
    /// reference to the IR context
    pub fn new(files: &'files Files, irctx: &'ctx mut IrContext, ctx: &'llvm Context) -> Self {
        let root = ctx.create_module("spark_module");
        Self {
            state: LLVMCodeGeneratorState {
                llvm_funs: irctx
                    .funs
                    .secondary(|(_, fun)| root.add_function(fun.name.as_str(), Self::gen_funtype(ctx, irctx, &fun.ty), Some(Linkage::External))),
                llvm_types: irctx
                    .types
                    .secondary(
                        |(_, ty)| Self::gen_type(ctx, irctx, ty)
                    ),
                llvm_vars: irctx
                    .vars
                    .secondary(|_| None),
                llvm_bbs: HashMap::new(),
                files,

                ctx,
                root,
                build: ctx.create_builder(),
            },
            irctx,
        }
    }
    
    /// Generate all LLVM bytecode for the given IR context and return the completed LLVM module
    pub fn gen(mut self, opts: CompileOpts) -> Module<'llvm> {
        for fun_id in self.state.llvm_funs.indices() {
            let fun = self.irctx.funs.get_secondary(fun_id);
            if let Some(body) = &fun.body {
                self.state.gen_bb(self.irctx, body.entry, self.state.llvm_funs[fun_id]);
            }
        }

        let fpm = PassManager::create(&self.state.root);
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();
        
        Target::initialize_native(&InitializationConfig::default()).unwrap();

        let opt = OptimizationLevel::default();
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();
        let target_machine = target
            .create_target_machine(
                &TargetMachine::get_default_triple(),
                TargetMachine::get_host_cpu_name().to_str().unwrap(),
                TargetMachine::get_host_cpu_features().to_str().unwrap(),
                opt,
                reloc,
                model
            )
            .unwrap();

        target_machine
            .write_to_file(
                &self.state.root,
                FileType::Object,
                &opts.out_file,
            )
            .unwrap();

        self.state.root
    }
    
    /// Translate integer types to LLVM 
    pub fn gen_inttype(ctx: &'llvm Context, ty: &IrIntegerType) -> IntType<'llvm> {
        match ty.width {
            IntegerWidth::Eight => ctx.i8_type(),
            IntegerWidth::Sixteen => ctx.i16_type(),
            IntegerWidth::ThirtyTwo => ctx.i32_type(),
            IntegerWidth::SixtyFour => ctx.i64_type(),
        }
    }
    
    /// Generate LLVM IR for a single IR type
    pub fn gen_type(ctx: &'llvm Context, irctx: &'ctx IrContext, ty: &IrType) -> BasicTypeEnum<'llvm> {
        match ty {
            IrType::Integer(ity) => Self::gen_inttype(ctx, ity).into(),
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
                if variants.is_empty() {
                    panic!("Type: {:#?} has no variants", ty);
                }
                let variants = variants
                    .iter()
                    .map(|variant| Self::gen_type(ctx, irctx, &irctx[*variant]))
                    .collect::<Vec<_>>();

                let largest_size = variants
                    .iter()
                    .map(|ty| ty
                        .size_of()
                        .unwrap()
                        .get_zero_extended_constant()
                        .unwrap_or(1)
                    )
                    .max()
                    .unwrap();

                ctx
                    .struct_type(&[ctx.i8_type().into(), ctx.i8_type().array_type(largest_size as u32).into()], false)
                    .into()
            },
            IrType::Array(ty, sz) => Self::gen_type(ctx, irctx, &irctx[*ty]).array_type(*sz as u32).into(),
            IrType::Alias { ty, .. } => Self::gen_type(ctx, irctx, &irctx[*ty]),
            IrType::Invalid => ctx.i8_type().into(),
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
