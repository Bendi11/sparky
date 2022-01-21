use std::{io::Write, path::Path};

use clap::{App, Arg};
use spark::{ast::{DefData, ParsedModule}, codegen::{ir::SparkCtx, lower::Lowerer}, parse::Parser, util::files::{CompiledFile, Files, FileId}, Symbol};


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
    let input = Path::new(args.value_of_os("input").unwrap());
    let mut files = Files::new();
    let input = collect_files(input, &mut files);

    let root_module = match input {
        InputItem::File(f) => {
            let src = files.get(f).text.as_str();
            let mut parser = Parser::new(src);
            let module = parser.parse(Symbol::from("root"), f).unwrap_or_else(|e| {
                for name in e.backtrace {
                    eprintln!("in {}", name)
                }

                eprintln!("{}", e.error);
                if let Some(span) = e.highlighted_span {
                    span.display(&files.get(f)).unwrap();
                }
                panic!()
            });
            drop(parser);
            drop(src);
            module
        }
        InputItem::Dir(name, items) => {
            let main = items.iter().find_map(|item| if let InputItem::File(id) = item {
                if files.get(*id).path.extension().map(|s| s.to_str()).flatten() == Some("sprk") &&
                    files.get(*id).path.file_name().map(|s| s.to_str()).flatten() == Some("main") {
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
            parser.parse_to(&mut root, main);

            for item in items {
                match item {
                    InputItem::File(f) if f == main => continue,
                    InputItem::File(f) => {
                        let src = files.get(f).text.as_str();
                        parser.set_text(src);
                        parser.parse_to(&mut root, f).unwrap_or_else(|e| {
                            for name in e.backtrace {
                                eprintln!("in {}", name)
                            }

                            eprintln!("{}", e.error);
                            if let Some(span) = e.highlighted_span {
                                span.display(&files.get(f)).unwrap();
                            }
                            panic!()
                        });
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
    
    lowerer.lower_module(&root_module);
}

fn parse_dir<'src>(name: String, items: Vec<InputItem>, files: &'src Files, parser: &mut Parser<'src>) -> ParsedModule {
    let mut root = ParsedModule::new(Symbol::from(&name));

    for item in items {
        match item {
            InputItem::File(f) => {
                let src = files.get(f).text.as_str();
                parser.set_text(src);
                parser.parse_to(&mut root, f).unwrap_or_else(|e| {
                    for name in e.backtrace {
                        eprintln!("in {}", name)
                    }

                    eprintln!("{}", e.error);
                    if let Some(span) = e.highlighted_span {
                        span.display(&files.get(f)).unwrap();
                    }
                    panic!()
                });
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
