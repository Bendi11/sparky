use std::path::Path;

use inkwell::{OptimizationLevel, module::Module, passes::PassManager, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target}};

use super::LlvmCodeGenerator;

impl<'ctx, 'files> LlvmCodeGenerator<'ctx, 'files> {
    ///Generate an object file from a compiled LLVM IR module
    pub fn gen_obj(&self, filename: &str, module: Module<'ctx>) {
        let opt = OptimizationLevel::Default;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let ofile = Path::new(filename);


        let passes = PassManager::create(&module);
        //passes.add_function_inlining_pass();
        passes.add_promote_memory_to_register_pass();
        passes.add_instruction_combining_pass();
        //passes.add_gvn_pass();
        passes.add_reassociate_pass();
        //passes.add_basic_alias_analysis_pass();
    
        passes.initialize();
    
        self.target.write_to_file(&module, FileType::Object, ofile).unwrap();
    }
}
