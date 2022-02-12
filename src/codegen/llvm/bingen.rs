use std::path::Path;

use inkwell::{module::Module, passes::PassManager, targets::FileType};

use crate::{OutputFileType, OutputOptimizationLevel};

use super::LlvmCodeGenerator;

impl<'ctx, 'files> LlvmCodeGenerator<'ctx, 'files> {
    ///Generate an object file from a compiled LLVM IR module
    pub fn finish(&self, module: Module<'ctx>) {
        let passes = PassManager::create(&module);

        if self.opts.opt_lvl >= OutputOptimizationLevel::Size {
            passes.add_cfg_simplification_pass();

            passes.add_instruction_combining_pass();
            passes.add_constant_merge_pass();
            passes.add_gvn_pass();
            passes.add_promote_memory_to_register_pass();
            passes.add_ind_var_simplify_pass();
        }

        if self.opts.opt_lvl == OutputOptimizationLevel::Size {
            passes.add_aggressive_dce_pass();
            passes.add_aggressive_inst_combiner_pass();
        }
        if self.opts.opt_lvl == OutputOptimizationLevel::Size || self.opts.stripped {
            passes.add_strip_symbol_pass();
        }

        if self.opts.opt_lvl >= OutputOptimizationLevel::Medium {
            //passes.add_function_inlining_pass();
            passes.add_loop_vectorize_pass();
            passes.add_loop_unroll_pass();
            passes.add_demote_memory_to_register_pass();
        }

        if self.opts.opt_lvl >= OutputOptimizationLevel::Release {
            passes.add_licm_pass();
            passes.add_merge_functions_pass();
            //passes.add_argument_promotion_pass();
        }

        passes.initialize();

        if let OutputFileType::LLVMIR = self.opts.out_type {
            module.print_to_file(&self.opts.out_file).unwrap();
            return;
        }

        self.target
            .write_to_file(
                &module,
                match self.opts.out_type {
                    OutputFileType::Assembly => FileType::Assembly,
                    OutputFileType::Object => FileType::Object,
                    OutputFileType::LLVMIR => unreachable!(),
                },
                &self.opts.out_file,
            )
            .unwrap();
    }
}
