

fn main() {
   test_parse() 
}

use std::io::Write;

use spark::{parse::Parser, util::files::{CompiledFile, Files}, ast::DefData};

    

    const SOURCE: &str = 
r#"
type test_ty i32 | bool
type test_struct {
    i32 field
}

fun test_fn i32 a -> () {
    let a := if true {
        phi "test\na" 
    } else {
        phi ""
    }
    let b := {
        let testing := a[0]
        phi testing
    }:(132)

    let(fun()->()) a := {}
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
        for (_, def) in module.defs.iter() {
            if let DefData::FunDef(_, body) = &def.data {
                for expr in body {
                    writeln!(&mut stdout, "{:#?}", expr.node);
                    writeln!(&mut stdout).unwrap();
                }
            }
        }
        
    }
