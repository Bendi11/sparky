use hashbrown::HashMap;
use inkwell::{AddressSpace, builder::Builder, context::Context, module::{Linkage, Module}, types::{
        BasicTypeEnum,
    }, values::{
        AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue,
        GlobalValue,
    }};

use crate::{ast::{Ast, Body, WordProto}, parse::Attr, types::StackLayout};

pub struct Codegen<'a> {
    pub ctx: &'a Context,
    pub builder: Builder<'a>,
    pub module: Module<'a>,

    /// A dictionary of names to function values
    pub dict: HashMap<String, (FunctionValue<'a>, WordProto)>,

    pub tree: Body,

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
    pub fn new(tree: Body, ctx: &'a Context) -> Self {
        let module = ctx.create_module("module");
        let mut this = Self {
            ctx,
            builder: ctx.create_builder(),
            module,
            tree,
            dict: HashMap::new(),
            stack: Vec::new(),
        };
        let add = this.module.add_function("add", this.ctx.i32_type().fn_type(&[BasicTypeEnum::IntType(this.ctx.i32_type())], false), Some(Linkage::External));
        this.dict.insert("add".to_owned(), (add, WordProto{
            input: StackLayout::new().with(crate::types::Type::Int{
                width: 32,
                signed: true
            }),
            output: StackLayout::new().with(crate::types::Type::Int{
                width: 32,
                signed: true
            }),
            name: "add".to_owned(),
            attrs: HashMap::new(),
        }));
        let bb = this.ctx.append_basic_block(add, "entry");
        this.builder.position_at_end(bb);
        this.builder.build_return(Some(&this.ctx.i32_type().const_int(100, false)));
        this
    }

    /// Generate code for one word call using arguments
    fn word_call(
        &self,
        word: String,
        args: &'a [BasicValueEnum],
    ) -> Result<CallSiteValue<'a>, CodegenErr> {
        match self.dict.get(&word) {
            Some((word, _)) => Ok(self.builder.build_call(*word, args, "word_call")),
            None => return Err(CodegenErr::UnknownWord(word)),
        }
    }

    /// Generate code for one AST node
    pub fn gen(&mut self, ast: Ast) -> Result<AnyValueEnum<'a>, CodegenErr> {
        use crate::types;
        match ast {
            //Word call codegen
            Ast::Word { path, attrs: _ } => {
                //Search our dictionary for the word
                match self.dict.get(&path) {
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
                    None => Err(CodegenErr::UnknownWord(path)),
                }
            }
            Ast::Literal { val } => match val.ty {
                types::Type::Int { width, signed } => {
                    let constant = self.ctx.i32_type().const_int(val.to_int() as u64, false);
                    self.stack.push(constant.as_basic_value_enum());
                    Ok(constant.as_any_value_enum())
                }
                _ => unimplemented!(),
            },

            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use inkwell::{module::Linkage, targets::{FileType, RelocMode, Target, TargetMachine}};

    use super::*;
    #[test]
    pub fn codegen() {
        let ctx = Context::create();
        let tree = "10 ".parse::<Body>().unwrap();
        let mut code = Codegen::new(tree, &ctx);

        let main_ty = ctx.i32_type().fn_type(&[], false);
        let main = code
            .module
            .add_function("entry", main_ty, Some(Linkage::External));
        code.builder
            .position_at_end(ctx.append_basic_block(main, "entry"));
        code.gen("10".parse::<Body>().unwrap().0[0].clone()).unwrap_or_else(|_| panic!("Can't codegen"));
        code.gen("add".parse::<Body>().unwrap().0[0].clone()).unwrap_or_else(|_| panic!("Can't codegen"));
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
