use std::{io::Write, path::Path};

use clap::{App, Arg};
use inkwell::context::Context;
use spark::{ast::{DefData, ParsedModule}, codegen::{ir::SparkCtx, lower::Lowerer, llvm::LlvmCodeGenerator}, parse::{Parser, ParseError}, util::files::{CompiledFile, Files, FileId}, Symbol, error::Diagnostic};


enum InputItem {
    Dir(String, Vec<InputItem>),
    File(FileId)
}

fn main() {
    let app = App::new("sparkc")
        .about("Compiler for the spark programming language")
        .arg(Arg::new("input")
            .required(true)
            .takes_value(true)
            .help("A path to an input file or directory to compile")
            .multiple_values(false)
            .value_name("input")
            .validator_os(|path| match Path::new(path).exists() {
                true => Ok(()),
                false => Err(format!("The input directory at {} does not exist", path.to_string_lossy()))
            })
        );

    let args = app.get_matches();
    let input = Path::new(args.value_of("input").unwrap());
    let mut files = Files::new();
    let input = collect_files(input, &mut files);

    let root_module = match input {
        InputItem::File(f) => {
            let src = files.get(f).text.as_str();
            let mut parser = Parser::new(src);
            let module = handle_parse_error(parser.parse(Symbol::from("root"), f), files.get(f));
            drop(parser);
            drop(src);
            module
        }
        InputItem::Dir(name, items) => {
            let main = items.iter().find_map(|item| if let InputItem::File(id) = item {
                if files.get(*id).path.file_name().map(|s| s.to_str()).flatten() == Some("main.sprk") {
                        Some(*id)
                    } else {
                        None
                    }
                } else {
                    None
                } 
            ).expect("main.sprk does not exist in root directory");
            let mut root = ParsedModule::new(Symbol::from("root"));
            let mut parser = Parser::new(files.get(main).text.as_str());
            handle_parse_error(parser.parse_to(&mut root, main), files.get(main));

            for item in items {
                match item {
                    InputItem::File(f) if f == main => continue,
                    InputItem::File(f) => {
                        let src = files.get(f).text.as_str();
                        parser.set_text(src);
                        handle_parse_error(parser.parse_to(&mut root, f), files.get(f));
                    },
                    InputItem::Dir(name, items) => {
                        let child = parse_dir(name.clone(), items, &files, &mut parser);
                        root.children.insert(Symbol::from(&name), child);
                    }
                }
            }
            root
        }
    };

    let mut ctx = SparkCtx::new();
    let mut lowerer = Lowerer::new(&mut ctx, &files);
    
    let root_id = lowerer.lower_module(&root_module);

    let mut llvm_ctx = Context::create();
    let mut generator = LlvmCodeGenerator::new(ctx, &mut llvm_ctx, &files);
    let llvm_root = generator.codegen_module(root_id);
    llvm_root.print_to_stderr();
}

fn handle_parse_error<T>(res: Result<T, ParseError>, file: &CompiledFile) -> T {
    res.unwrap_or_else(|e| {
        for name in e.backtrace {
            eprintln!("in {}", name)
        }

        eprintln!("{}", e.error);
        if let Some(span) = e.highlighted_span {
            span.display(&file).unwrap();
        }

        eprintln!();
        
        std::process::exit(-1);
    })

}

fn parse_dir<'src>(name: String, items: Vec<InputItem>, files: &'src Files, parser: &mut Parser<'src>) -> ParsedModule {
    let mut root = ParsedModule::new(Symbol::from(&name));

    for item in items {
        match item {
            InputItem::File(f) => {
                let src = files.get(f).text.as_str();
                parser.set_text(src);
                handle_parse_error(parser.parse_to(&mut root, f), files.get(f));
            },
            InputItem::Dir(name, items) => {
                let child = parse_dir(name.clone(), items, files, parser);
                root.children.insert(Symbol::new(name), child);
            }
        }
    }
    root
}


/// Collect all input items from a file or directory
fn collect_files(input: &Path, files: &mut Files) -> InputItem {
    match input.is_dir() {
        true => {
            let mut items = vec![];
            for entry in input.read_dir().expect("failed to read directory") {
                if let Ok(entry) = entry {
                    let item = collect_files(&entry.path(), files);
                    items.push(item);
                }
            }
            InputItem::Dir(input.to_string_lossy().into_owned(), items)
        },
        false => {
            let id = files.add(CompiledFile::open(input).expect("failed to open a file"));
            InputItem::File(id)
        }
    }
}
