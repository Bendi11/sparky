use hashbrown::HashMap;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum, FunctionType, IntType},
    values::{FunctionValue, PointerValue, BasicValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    arena::Arena,
    ast::{FunFlags, IntegerWidth},
    ir::{
        types::{FunType, IrFloatType, IrIntegerType, IrType},
        BBId, IrContext,
    },
    CompileOpts, OutputFileType, OutputOptimizationLevel,
};

pub mod expr;
pub mod stmt;

/// Structure containing all state needed to generate LLVM IR from spark IR
pub struct LLVMCodeGenerator<'ctx, 'llvm> {
    state: LLVMCodeGeneratorState<'llvm>,
    irctx: &'ctx mut IrContext,
}

pub struct LLVMCodeGeneratorState<'llvm> {
    ctx: &'llvm Context,
    root: Module<'llvm>,
    build: Builder<'llvm>,
    llvm_funs: Arena<FunctionValue<'llvm>>,
    llvm_types: Arena<BasicTypeEnum<'llvm>>,
    llvm_vars: Arena<Option<PointerValue<'llvm>>>,
    llvm_bbs: HashMap<BBId, BasicBlock<'llvm>>,
}

impl<'ctx, 'llvm> LLVMCodeGenerator<'ctx, 'llvm> {
    /// Create a new [LLVMCodeGenerator] from shared reference to a [Files] structure and unique
    /// reference to the IR context
    pub fn new(irctx: &'ctx mut IrContext, ctx: &'llvm Context) -> Self {
        let root = ctx.create_module("spark_module");
        let mut id = 0;
        Self {
            state: LLVMCodeGeneratorState {
                llvm_funs: irctx.funs.secondary(|(_, fun)| {
                    root.add_function(
                        if fun.flags.contains(FunFlags::EXTERN) {
                            fun.name.to_string()
                        } else {
                            id += 1;
                            format!("{}#{}", fun.name, id)
                        }
                        .as_str(),
                        Self::gen_funtype(ctx, irctx, &fun.ty),
                        Some(Linkage::External),
                    )
                }),
                llvm_types: irctx
                    .types
                    .secondary(|(_, ty)| Self::gen_type(ctx, irctx, ty)),
                llvm_vars: irctx.vars.secondary(|_| None),
                llvm_bbs: HashMap::new(),
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
            let llvm_fun = self.state.llvm_funs[fun_id];
            if let Some(body) = &fun.body {
                let bb = self.state.ctx.append_basic_block(llvm_fun, "entry");
                self.state.llvm_bbs.insert(body.entry, bb);
                self.state.build.position_at_end(bb);
                for (idx, (ty, param)) in fun.ty.params.iter().enumerate() {
                    if let Some(name) = param {
                        let alloca = self.state.build.build_alloca(*self.state.llvm_types.get_secondary(*ty), name.as_str());
                        self.state.build.build_store(alloca, llvm_fun.get_nth_param(idx as u32).unwrap());
                        *self.state.llvm_vars.get_secondary_mut(body.args[idx].unwrap()) = Some(alloca);
                    }
                }
                self.state
                    .gen_bb(self.irctx, body.entry, self.state.llvm_funs[fun_id]);
            }
        }

        let fpm = PassManager::create(&self.state.root);
        if opts.opt_lvl > OutputOptimizationLevel::Debug {
            fpm.add_instruction_combining_pass();
            fpm.add_reassociate_pass();
            fpm.add_gvn_pass();
            fpm.add_cfg_simplification_pass();
            fpm.add_basic_alias_analysis_pass();
            fpm.add_promote_memory_to_register_pass();
            fpm.add_instruction_combining_pass();
            fpm.add_reassociate_pass();
        }

        if opts.stripped {
            fpm.add_strip_symbol_pass();
        }

        fpm.finalize();

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        let opt = match opts.opt_lvl {
            OutputOptimizationLevel::Release => OptimizationLevel::Aggressive,
            OutputOptimizationLevel::Medium => OptimizationLevel::Default,
            OutputOptimizationLevel::Size => OptimizationLevel::Less,
            OutputOptimizationLevel::Debug => OptimizationLevel::None,
        };
        let reloc = match opts.pic {
            true => RelocMode::PIC,
            false => RelocMode::Default,
        };
        let model = CodeModel::Default;
        let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();
        let target_machine = target
            .create_target_machine(
                &TargetMachine::get_default_triple(),
                TargetMachine::get_host_cpu_name().to_str().unwrap(),
                TargetMachine::get_host_cpu_features().to_str().unwrap(),
                opt,
                reloc,
                model,
            )
            .unwrap();

        match opts.out_type {
            OutputFileType::Object => {
                target_machine.write_to_file(&self.state.root, FileType::Object, &opts.out_file)
            }
            OutputFileType::Assembly => {
                target_machine.write_to_file(&self.state.root, FileType::Assembly, &opts.out_file)
            }
            OutputFileType::LLVMIR => self.state.root.print_to_file(&opts.out_file),
            OutputFileType::IR => unreachable!(),
        }
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
    pub fn gen_type(
        ctx: &'llvm Context,
        irctx: &'ctx IrContext,
        ty: &IrType,
    ) -> BasicTypeEnum<'llvm> {
        match ty {
            IrType::Integer(ity) => Self::gen_inttype(ctx, ity).into(),
            IrType::Float(IrFloatType { doublewide }) => match doublewide {
                true => ctx.f64_type().into(),
                false => ctx.f32_type().into(),
            },
            IrType::Bool => ctx.bool_type().into(),
            IrType::Unit => ctx.i8_type().into(),
            IrType::Ptr(ty) => Self::gen_type(ctx, irctx, &irctx[*ty])
                .ptr_type(AddressSpace::Generic)
                .into(),
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
            }
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
                    .map(|ty| {
                        ty.size_of()
                            .unwrap()
                            .get_zero_extended_constant()
                            .unwrap_or(1)
                    })
                    .max()
                    .unwrap();

                ctx.struct_type(
                    &[
                        ctx.i8_type().into(),
                        ctx.i8_type().array_type(largest_size as u32).into(),
                    ],
                    false,
                )
                .into()
            }
            IrType::Array(ty, sz) => Self::gen_type(ctx, irctx, &irctx[*ty])
                .array_type(*sz as u32)
                .into(),
            IrType::Alias { ty, .. } => Self::gen_type(ctx, irctx, &irctx[*ty]),
            IrType::Invalid => ctx.i8_type().into(),
        }
    }

    /// Generate the LLVM IR signature for the given IR function signature
    fn gen_funtype(
        ctx: &'llvm Context,
        irctx: &'ctx IrContext,
        ty: &FunType,
    ) -> FunctionType<'llvm> {
        let return_ty = Self::gen_type(ctx, irctx, &irctx[ty.return_ty]);
        let params = ty
            .params
            .iter()
            .map(|(ty, _)| Self::gen_type(ctx, irctx, &irctx[*ty]).into())
            .collect::<Vec<_>>();

        return_ty.fn_type(&params, false)
    }
}
