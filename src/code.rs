use hashbrown::HashMap;
use inkwell::{builder::Builder, context::Context, module::Module, values::{
        AnyValue, AnyValueEnum, BasicValueEnum, FunctionValue,
    }};

use crate::ast::{Ast, Body, WordProto};

pub struct Codegen<'a> {
    pub ctx: &'a Context,
    pub builder: Builder<'a>,
    pub module: Module<'a>,

    /// A dictionary of names to function values
    pub dict: HashMap<String, (FunctionValue<'a>, WordProto<'a>)>,

    pub tree: Body<'a>,

    /// A stack of basicvalues, there is no real stack in LLVM so we make Allocas and keep track of them
    /// in the stack of the compiler, then when wanting to pop from the stack, you pop an alloca instance from it.
    /// The top of the stack is always at the highest index
    pub stack: Vec<BasicValueEnum<'a>>,
}

/// Every type of error that can occur in code generation
pub enum CodegenErr {
    /// Unknown word call
    UnknownWord(String),
}

impl<'a> Codegen<'a> {
    /// Create a new code generator from an AST and a compilation context
    pub fn new(tree: Body<'a>, ctx: &'a Context) -> Self {
        let module = ctx.create_module("module");
        Self {
            ctx,
            builder: ctx.create_builder(),
            module,
            tree,
            dict: HashMap::new(),
            stack: Vec::new(),
        }
    }

    /// Generate code for one AST node
    pub fn gen(&mut self, idx: usize) -> Result<AnyValueEnum<'a>, CodegenErr> {
        match &self.tree[idx] {
            //Word call codegen
            Ast::Word { ref path, attrs: _ } => {
                //Search our dictionary for the word
                match self.dict.get(path) {
                    Some((word, proto)) => {
                        //Create the call to the function using our stack
                        Ok(self
                            .builder
                            .build_call(
                                *word,
                                self.stack
                                    .drain(self.stack.len() - proto.input.len()..)
                                    .as_slice(),
                                "word_call",
                            )
                            .as_any_value_enum()) //Take the amount of items from the stack
                    }
                    None => Err(CodegenErr::UnknownWord(path.to_owned())),
                }
            }
            Ast::Literal { val } => Ok(val.as_any_value_enum()),

            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use inkwell::{module::Linkage, targets::{FileType, RelocMode, Target, TargetMachine}};

    use crate::parse::SparkParse;

    use super::*;
    #[test]
    pub fn codegen() {
        let ctx = Context::create();
        let tree = "10 add".spark_parse(&ctx).unwrap();
        let mut code = Codegen::new(tree, &ctx);

        let main_ty = ctx.i32_type().fn_type(&[], false);
        let main = code
            .module
            .add_function("entry", main_ty, Some(Linkage::External));
        code.builder
            .position_at_end(ctx.append_basic_block(main, "entry"));
        code.gen(0).unwrap_or_else(|_| panic!("Can't codegen"));
        code.gen(1).unwrap_or_else(|_| panic!("Can't codegen"));
        code.builder.build_return(None);

        code.module.print_to_file("./test-out.il").unwrap();
        Target::initialize_all(&Default::default());
        let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();
        let machine = unsafe {
             target.create_target_machine(&TargetMachine::get_default_triple(), std::str::from_utf8_unchecked(TargetMachine::get_host_cpu_name().to_bytes()), std::str::from_utf8_unchecked(TargetMachine::get_host_cpu_features().to_bytes()), inkwell::OptimizationLevel::None, RelocMode::Default, inkwell::targets::CodeModel::Default).unwrap()
        };
        machine.write_to_file(&code.module, FileType::Assembly, std::path::Path::new("./o.asm")).unwrap();
        machine.write_to_file(&code.module, FileType::Object, std::path::Path::new("./o.obj")).unwrap();
    }
}
