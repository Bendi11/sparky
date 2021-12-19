pub mod parse;
pub mod util;
pub mod ast;

#[cfg(test)]
mod tests {
    use string_interner::StringInterner;

    use crate::{parse::Parser, util::files::CompiledFile};

    

    const SOURCE: &str = 
r#"
type test_ty i32 | bool
type test_struct {
    i32 field
}

fun test_fn {
    let a := if true {
        
    } else {

    }
}

"#;
    
    #[test]
    pub fn test_parse() {
        let file = CompiledFile::in_memory(SOURCE.to_owned());
        let mut interner = StringInterner::new();
        let mut parser = Parser::new(SOURCE, &mut interner, "buffer");
        let module = parser.parse().unwrap_or_else(|e| {
            for name in e.backtrace {
                eprintln!("in {}", name)
            }

            eprintln!("{}", e.error);
            if let Some(span) = e.highlighted_span {
                span.display(&file).unwrap();
            }
            panic!()
        });
        println!("{:#?}", module);
        panic!()
    }
}
