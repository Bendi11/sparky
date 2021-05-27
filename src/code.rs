use std::convert::TryInto;

use inkwell::{AddressSpace, builder::Builder, context::Context, module::{Linkage, Module}, types::BasicType, values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode}};

use crate::ast::{Ast, Body, FnProto};

/// The `Compiler` struct is in charge of generating code and invoking the native linker to produce a finished executable
/// from a list of root [AST](crate::ast::Ast) nodes
pub struct Compiler<'ctx> {
    /// The compilation context
    pub ctx: &'ctx Context,

    /// The llvm module that we are generating code for
    pub module: Module<'ctx>,

    /// The IR builder that we use to actually generate LLVM IR that can then be compiled to a native object file
    pub builder: Builder<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    /// Create a new `Compiler` from an LLVM compilation context
    pub fn new(ctx: &'ctx Context) -> Self {
        Self {
            module: ctx.create_module("spark_module"),
            builder: ctx.create_builder(),
            ctx,
        }
    }

    /// Generate code for all expressions, returning the completed LLVM module
    pub fn gen_all(mut self, tree: Body<'ctx>) -> Result<Module<'ctx>, Box<dyn std::error::Error>> {
        for expr in tree.into_iter() {
            self.gen(expr)?;
        }

        self.module.verify()?;
        Ok(self.module)
    }

    /// Generate code for a function prototype
    pub fn gen_fn_proto(
        &mut self,
        proto: FnProto<'ctx>,
    ) -> Result<FunctionValue<'ctx>, Box<dyn std::error::Error>> {
        Ok(self.module.add_function(
            proto.name.as_str(),
            proto.ret.fn_type(proto.args.as_slice(), false),
            None,
        ))
    }

    fn gen(&mut self, ast: Ast<'ctx>) -> Result<AnyValueEnum<'ctx>, Box<dyn std::error::Error>> {
        use std::convert::TryFrom;
        match ast {
            Ast::FnProto(proto) => Ok(self.gen_fn_proto(proto)?.as_any_value_enum()),

            //Generate a function call
            Ast::Call { name, args } => match self.module.get_function(name.as_str()) {
                Some(func) => {
                    let mut arg_values: Vec<BasicValueEnum<'ctx>> = Vec::new();
                    for expr in args {
                        arg_values.push(self.gen(expr)?.try_into().unwrap())
                    }

                    Ok(self
                        .builder
                        .build_call(func, arg_values.as_slice(), name.as_str())
                        .as_any_value_enum())
                }
                None => return Err(format!("Calling unknown function {}", name))?,
            },
            Ast::StrLiteral(literal) => {
                let global = self.ctx.i8_type().array_type((literal.len() + 1) as u32);
                let string = self.module.add_global(global, None, "global_str");
                string.set_initializer(&self.ctx.const_string(literal.as_bytes(), true));

                Ok( self.builder.build_cast(InstructionOpcode::BitCast, string, self.ctx.i8_type().ptr_type(AddressSpace::Generic), "string_cast").as_any_value_enum() )
                
            }
            Ast::NumLiteral(literal) => Ok(literal.as_any_value_enum()),
            Ast::Ret(val) => {
                let val = BasicValueEnum::try_from(self.gen(*val)?).unwrap();
                Ok(self.builder.build_return(Some(&val)).as_any_value_enum())
            }
            Ast::Fn(proto, body) => Ok(self.gen_fn_decl(proto, body)?.as_any_value_enum()),
            _ => unimplemented!(),
        }
    }

    /// Generate code for a function definition with body statements
    pub fn gen_fn_decl(
        &mut self,
        proto: FnProto<'ctx>,
        body: Body<'ctx>,
    ) -> Result<FunctionValue<'ctx>, Box<dyn std::error::Error>> {
        //Check if the function was already declared
        match self.module.get_function(proto.name.as_str()) {
            Some(f) => {
                //TODO: make sure a function isn't defined twice
                let entry = self.ctx.append_basic_block(f, "entry"); //Create an entry basic block
                self.builder.position_at_end(entry); //Position the instruction inserter into the function
                for expr in body {
                    self.gen(expr)?;
                }
                Ok(f)
            }
            None => {
                let f = self.gen_fn_proto(proto)?;
                let entry = self.ctx.append_basic_block(f, "entry"); //Create an entry basic block
                self.builder.position_at_end(entry); //Position the instruction inserter into the function
                for expr in body {
                    self.gen(expr)?;
                }
                Ok(f)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lex::Lexer, parse::Parser};
    use std::io::BufReader;

    #[test]
    pub fn codegen() {
        let ctx = Context::create();

        const SRC: &[u8] = br#"
        fun ext test(i8 ptr argv) i8;

        fun testing() i32 {
            test("testing");
            ret 100;
        };
        "#;

        let mut reader = BufReader::new(SRC);
        let mut parser: Parser<'_, Lexer<_>> = Parser::new(&ctx, Lexer::from_reader(&mut reader));
        let gen = Compiler::new(&ctx);
        let module = gen.gen_all(parser.parse().unwrap()).unwrap();
        module.print_to_file("./o.ll").unwrap();
    }
}
