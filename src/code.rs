use hashbrown::HashMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{AnyValue, AnyValueEnum, BasicValueEnum, CallSiteValue, FunctionValue, GlobalValue},
    AddressSpace,
};

use crate::ast::{Ast, Body, WordProto};

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
        Self {
            ctx,
            builder: ctx.create_builder(),
            module,
            tree,
            dict: HashMap::new(),
            stack: Vec::new(),
        }
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
    fn gen(&mut self, ast: Ast) -> Result<AnyValueEnum<'a>, CodegenErr> {
        use crate::types;
        match ast {
            //Word call codegen
            Ast::Word { path, attrs: _ } => {
                //Search our dictionary for the word
                match self.dict.get(&path) {
                    Some((word, proto)) => {
                        //Create the call to the function using our stack
                        Ok(self.builder.build_call(
                            *word,
                            self.stack
                                .drain(self.stack.len() - proto.input.len()..)
                                .as_slice(),
                            "word_call",
                        ).as_any_value_enum())//Take the amount of items from the stack
                    }
                    None => Err(CodegenErr::UnknownWord(path)),
                }
            },
            Ast::Literal{val} => match val.ty {
                types::Type::Int{width, signed} => {
                    
                },
                _ => unimplemented!(),
            },

            _ => unimplemented!(),
        }
    }
}
