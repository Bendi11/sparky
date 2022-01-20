

fn main() {
   test_parse() 
}

use std::io::Write;

use spark::{ast::DefData, codegen::{ir::SparkCtx, lower::Lowerer}, parse::Parser, util::files::{CompiledFile, Files}};

const SOURCE: &str = 
r#"
type test_ty i32 | bool
type test_struct {
    test_ty field
    [15]*u8 other_field
}

fun test_fn i32 a -> () {
    let(test_struct) a := 0
    a.field := 1000
    a.other_field[1] := 0xFFFF
}

"#;
    
pub fn test_parse() {
    let mut files = Files::new();
    let file = files.add(CompiledFile::in_memory(SOURCE.to_owned()));
    let mut parser = Parser::new(SOURCE, "buffer", file);
    let module = parser.parse().unwrap_or_else(|e| {
        for name in e.backtrace {
            eprintln!("in {}", name)
        }

        eprintln!("{}", e.error);
        if let Some(span) = e.highlighted_span {
            span.display(&files.get(file)).unwrap();
        }
        panic!()
    });

    let mut stdout = std::io::stdout();

    let mut ctx = SparkCtx::new(file);
    let mut lowerer = Lowerer::new(&mut ctx, &files);
    
    lowerer.lower_module(&module);
    drop(lowerer);
    println!("{:#?}", ctx);

    for (_, def) in module.defs.iter() {
        if let DefData::FunDef(_, body) = &def.data {
            for expr in body {
                writeln!(&mut stdout, "{:#?}", expr.node);
                writeln!(&mut stdout).unwrap();
            }
        }
    }
    
}
