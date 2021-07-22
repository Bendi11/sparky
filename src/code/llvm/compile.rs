use std::{path::Path, process::Command};

use inkwell::{OptimizationLevel, module::Module, passes::PassManager, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}};

use crate::{CompileOpts, OutFormat, ast::{Ast, AstPos}, code::linker::Linker};

use super::Compiler;


impl<'c> Compiler<'c> {
    /// Generate all code for a LLVM module and return it
    pub fn finish(mut self, ast: Vec<AstPos>) -> Module<'c> {
        let ast = self.scan_decls(ast);
        //let ast = self.get_fn_protos(ast);
        for node in ast {
            match node.ast() {
                Ast::FunDef(ref proto, ref body) => self.gen_fundef(proto, body),
                other => panic!("Invalid top level expression {:?}", other),
            }
        }
        self.module
    }

    /// Compile the code into an executable / library file
    pub fn compile<L: Linker>(self, ast: Vec<AstPos>, opts: CompileOpts, mut linker: L) {
        const LINKER: &str = "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.28.29910\\bin\\Hostx64\\x64\\link.exe";
        use std::process::Stdio;

        let module = self.finish(ast);

        module
            .verify()
            .unwrap_or_else(|e| panic!("Failed to verify the LLVM module: {}", e));

        let fpm: PassManager<Module<'c>> = PassManager::create(());

        match opts.opt_lvl {
            crate::OptLvl::Debug => (),
            crate::OptLvl::Medium => {
                fpm.add_demote_memory_to_register_pass();
                fpm.add_promote_memory_to_register_pass();
                fpm.add_constant_merge_pass();
                fpm.add_instruction_combining_pass();
                fpm.add_global_optimizer_pass();
            }
            crate::OptLvl::Aggressive => {
                fpm.add_demote_memory_to_register_pass();
                fpm.add_promote_memory_to_register_pass();
                fpm.add_constant_merge_pass();
                fpm.add_instruction_combining_pass();
                fpm.add_global_optimizer_pass();

                fpm.add_loop_rotate_pass();
                fpm.add_argument_promotion_pass();
                fpm.add_function_inlining_pass();
                fpm.add_memcpy_optimize_pass();
                fpm.add_loop_deletion_pass();
                fpm.add_loop_vectorize_pass();
                fpm.add_constant_propagation_pass();
                fpm.add_simplify_lib_calls_pass();
                fpm.add_strip_symbol_pass();
            }
        }

        match opts.output_ty {
            OutFormat::IR => module.print_to_file(opts.out_file).unwrap(),
            other => {
                Target::initialize_all(&InitializationConfig::default());
                let opt = OptimizationLevel::Aggressive;
                let reloc = RelocMode::Default;
                let model = CodeModel::Default;
                let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();
                let machine = target
                    .create_target_machine(
                        &TargetMachine::get_default_triple(),
                        &TargetMachine::get_host_cpu_name().to_str().unwrap(),
                        &TargetMachine::get_host_cpu_features().to_str().unwrap(),
                        opt,
                        reloc,
                        model,
                    )
                    .unwrap();

                machine.add_analysis_passes(&fpm);

                match other {
                    OutFormat::Asm => machine
                        .write_to_file(&module, FileType::Assembly, &opts.out_file)
                        .unwrap(),
                    OutFormat::Obj => machine
                        .write_to_file(&module, FileType::Object, &opts.out_file)
                        .unwrap(),
                    OutFormat::Lib => {
                        let obj = opts.out_file.with_extension("obj");
                        machine
                            .write_to_file(&module, FileType::Object, &obj)
                            .unwrap();

                        let out = format!("/OUT:{}", opts.out_file.display());
                        let mut args = vec!["/LIB", obj.to_str().unwrap(), "/NOLOGO", out.as_str()];
                        args.extend(opts.libraries.iter().map(|s| s.as_str())); //Add all linked libraries

                        let cmd = Command::new(Path::new(LINKER))
                            .args(args)
                            .stderr(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn()
                            .unwrap(); //Link the file into a library
                        println!(
                            "{}",
                            String::from_utf8(cmd.wait_with_output().unwrap().stdout).unwrap()
                        );

                        std::fs::remove_file(obj).unwrap();
                    }
                    OutFormat::Exe => {
                        let obj = opts.out_file.with_extension("obj");
                        machine
                            .write_to_file(&module, FileType::Object, &obj)
                            .unwrap();

                        linker.add_object_file(obj.to_str().unwrap().to_owned());
                        linker.set_format(OutFormat::Exe);
                        linker.set_entry(Some("main"));
                        linker.set_output_file(opts.out_file);
                        linker.link().unwrap();

                        
                        std::fs::remove_file(obj).unwrap();
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}