use std::convert::TryInto;

use inkwell::{AddressSpace, builder::Builder, context::Context, module::{Linkage, Module}, types::{BasicType, BasicTypeEnum}, values::{
        AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode,
    }};

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
    
    /// The current function that we are generating code in
    current_fn: Option<&'ctx FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    /// Create a new `Compiler` from an LLVM compilation context
    pub fn new(ctx: &'ctx Context) -> Self {
        Self {
            module: ctx.create_module("spark_module"),
            builder: ctx.create_builder(),
            ctx,
            current_fn: None
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

    /// Create an entry alloca for mutable variables
    fn create_entry_alloca(&mut self, f: FunctionValue<'ctx>, name: String, ty: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
        let mut entry_builder = self.ctx.create_builder();
        entry_builder.position_at(f.get_first_basic_block().unwrap(), &f.get_first_basic_block().unwrap().get_first_instruction().unwrap());
        return entry_builder.build_alloca(ty, name).as_basic_value_enum();
    }

    /// Generate code for a binary expression
    pub fn gen_bin(&mut self, lhs: Box<Ast<'ctx>>, rhs: Box<Ast<'ctx>>, op: String) -> Result<BasicValueEnum<'ctx>, Box<dyn std::error::Error>> {
        //If lhs is a var declaration, then only an assignment operator can be used
        if let Ast::VarDecl{name, ty, attrs} = lhs {
            let var: BasicValueEnum<'ctx> = self.gen(lhs.into())?.try_into()?;
            let rhs: BasicValueEnum<'ctx> = self.gen(rhs.into()).try_into()?;
            if ty != rhs.get_type() {
                return Err(format!("Variable type does not match expression type being assigned!"))
            }

            if op.as_str() != "=" {
                return Err(format!("Variable declaration can only be assigned, operator {} used", op))
            }
            
        }
        let lhs: BasicValueEnum<'ctx> = self.gen(lhs.into())?.try_into()?;
        let rhs: BasicValueEnum<'ctx> = self.gen(rhs.into())?.try_into()?;
        if lhs.get_type() != rhs.get_type() {
            return Err(format!("Left hand side of binary expression does not match types with right hand side"))
        }
        let ty = rhs.get_type();
        match ty {
            BasicTypeEnum::IntType(ty) => match op.as_str() {
                "+" => Ok(self.builder.build_int_add(lhs, rhs, "tmp_iadd")),
                "-" => Ok(self.builder.build_int_sub(lhs, rhs, "tmp_isub")),
                "*" => Ok(self.builder.build_int_mul(lhs, rhs, "tmp_imul")),
                "/" => Ok(self.builder.build_int_signed_div(lhs, rhs, "tmp_isigneddiv")),
                unknown => return Err(format!("Unknown operator {} on integer types!", unknown)),
            },
            _ => return Err(format!("Type of binary expression is {}, and no binary operators can be applied to types of {}", ty, ty))
        }
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

                Ok(self
                    .builder
                    .build_cast(
                        InstructionOpcode::BitCast,
                        string,
                        self.ctx.i8_type().ptr_type(AddressSpace::Generic),
                        "string_cast",
                    )
                    .as_any_value_enum())
            }
            Ast::NumLiteral(literal) => Ok(literal.as_any_value_enum()),
            Ast::Ret(val) => {
                let val = BasicValueEnum::try_from(self.gen(*val)?).unwrap();
                Ok(self.builder.build_return(Some(&val)).as_any_value_enum())
            },
            Ast::VarDecl{name, attrs, ty} => {

            },
            Ast::Fn(proto, body) => Ok(self.gen_fn_decl(proto, body)?.as_any_value_enum()),
            Ast::Binary{lhs, rhs, op} => Ok(self.gen_bin(*lhs, *rhs, op)?),
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
                self.current_fn = Some(&f);
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
                self.current_fn = Some(&f);
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
    use inkwell::{OptimizationLevel, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple}};

    use super::*;
    use crate::{lex::Lexer, parse::Parser};
    use std::{io::BufReader, path::Path};

    #[test]
    pub fn codegen() {
        let ctx = Context::create();

        const SRC: &[u8] = br#"
        fun ext printf(u8 ptr fmt, i32 arg) i8;

        fun testing() i32 {
            printf("testing %d", 100);
            ret 100;
        };
        "#;

        let mut reader = BufReader::new(SRC);
        let mut parser: Parser<'_, Lexer<_>> = Parser::new(&ctx, Lexer::from_reader(&mut reader));
        let gen = Compiler::new(&ctx);
        let module = gen.gen_all(parser.parse().unwrap()).unwrap();
        module.print_to_file("./o.ll").unwrap();

        Target::initialize_all(&InitializationConfig::default());

        let opt = OptimizationLevel::Aggressive;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();
        let machine = target.create_target_machine(
            &TargetMachine::get_default_triple(),
            TargetMachine::get_host_cpu_name().to_str().unwrap(),
            TargetMachine::get_host_cpu_features().to_str().unwrap(),
            opt,
            reloc,
            model,
        ).unwrap();

        machine.write_to_file(&module, FileType::Assembly, Path::new("./o.s")).unwrap();
        machine.write_to_file(&module, FileType::Object, Path::new("./o.obj")).unwrap();
    }
}
