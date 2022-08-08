use std::path::{Path, PathBuf};

use clap::{App, Arg, ValueHint};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use inkwell::context::Context;
use spark::{
    ast::ParsedModule,
    error::DiagnosticManager,
    ir::{lower::IrLowerer, IrContext},
    llvm::LLVMCodeGenerator,
    parse::{ParseError, Parser},
    util::files::{CompiledFile, FileId, Files},
    CompileOpts, OutputFileType, OutputOptimizationLevel, Symbol,
};

/// Input source code, either a file or a directory containing source files
enum InputItem {
    /// A directory with name containing more files
    Dir(String, Vec<InputItem>),
    /// File to be parsed and compiled into a part of a spark module
    File(FileId),
}

fn main() {
    let app = App::new("sparkc")
        .about("Compiler for the spark programming language")
        .arg(
            Arg::new("input-path")
                .required(true)
                .takes_value(true)
                .help("A path to an input file or directory to compile")
                .long_help("A full or relative path to a file or directory to compile.\nIf a directory is passed, all files ending in .sprk will be compiled")
                .multiple_values(false)
                .value_name("input")
                .validator_os(|path| match Path::new(path).exists() {
                    true => Ok(()),
                    false => Err(format!(
                        "The input directory at {} does not exist",
                        path.to_string_lossy()
                    )),
                })
                .value_hint(ValueHint::AnyPath)
                .help_heading("input"),
        )
        .arg(Arg::new("opt-lvl")
            .short('O')
            .long("opt-lvl")
            .takes_value(true)
            .default_value("0")
            .possible_values([
                "0", "1", "2", "size"
            ])
            .value_name("optimization-level")
            .help("Set the optimization level of the output")
            .help_heading("output")
        )
        .arg(Arg::new("output-file")
            .short('o')
            .long("output-file")
            .takes_value(true)
            .help("Path to a file that will have output written to")
            .help_heading("output")
            .required(true)
        )
        .arg(Arg::new("output-type")
            .short('T')
            .long("output-type")
            .takes_value(true)
            .possible_values([
                "asm",
                "obj",
                "ll"
            ])
            .help("Set the output type to be written to the output file")
            .help_heading("output")
            .long_help("Explicitly set the output file type instead of guessing from the extension given to [output-file]")
        )
        .arg(Arg::new("pic")
            .long("pic")
            .help("Generate position independent output")
            .help_heading("output")
            .takes_value(false)
        )
        .arg(Arg::new("strip")
            .long("strip")
            .takes_value(false)
            .help("Strip symbols from the produced output (redundant if -Osize is passed)")
            .help_heading("output")
        );

    let args = app.get_matches();

    let opts = CompileOpts {
        out_file: PathBuf::from(args.value_of("output-file").unwrap()),
        out_type: match args.value_of("output-type") {
            Some(ty) => match ty {
                _ => unreachable!(),
            },
            None => match Path::new(args.value_of("output-file").unwrap()).extension() {
                Some(ext) => match ext.to_str() {
                    Some("obj") | Some("o") => OutputFileType::Object,
                    Some("ll") => OutputFileType::LLVMIR,
                    Some("asm") | Some("s") => OutputFileType::Assembly,
                    _ => {
                        eprintln!(
                            "Output file '{}' has an unknown extension\nUse -T[type] option to explicitly set output type",
                            args.value_of("output-file").unwrap(),
                        );
                        return;
                    }
                },
                None => {
                    eprintln!(
                        "Output file '{}' has no extension\nUse -T[type] option to explicitly set output type",
                        args.value_of("output-file").unwrap(),
                    );
                    return;
                }
            },
        },
        opt_lvl: match args.value_of("opt-lvl").unwrap() {
            "0" => OutputOptimizationLevel::Debug,
            "1" => OutputOptimizationLevel::Medium,
            "2" => OutputOptimizationLevel::Release,
            "size" => OutputOptimizationLevel::Size,
            _ => unreachable!(),
        },
        pic: args.is_present("pic"),
        stripped: args.is_present("strip"),
    };

    let input = Path::new(args.value_of("input-path").unwrap());
    let mut files = Files::new();
    let input = collect_files(input, &mut files);

    let root_module = match input {
        InputItem::File(f) => {
            let src = files.get(f).text.as_str();
            let mut parser = Parser::new(src);
            let module = handle_parse_error(parser.parse(Symbol::from("root"), f), &files, f);
            drop(parser);
            drop(src);
            module
        }
        InputItem::Dir(_name, items) => {
            let main = items
                .iter()
                .find_map(|item| {
                    if let InputItem::File(id) = item {
                        if files
                            .get(*id)
                            .path
                            .file_name()
                            .map(|s| s.to_str())
                            .flatten()
                            == Some("main.sprk")
                        {
                            Some(*id)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .expect("main.sprk does not exist in root directory");
            let mut root = ParsedModule::new(Symbol::from("root"));
            let mut parser = Parser::new(files.get(main).text.as_str());
            handle_parse_error(parser.parse_to(&mut root, main), &files, main);

            for item in items {
                match item {
                    InputItem::File(f) if f == main => continue,
                    InputItem::File(f) => {
                        let src = files.get(f).text.as_str();
                        parser.set_text(src);
                        handle_parse_error(parser.parse_to(&mut root, f), &files, f);
                    }
                    InputItem::Dir(name, items) => {
                        let child = parse_dir(name.clone(), items, &files, &mut parser);
                        root.children.push(child);
                    }
                }
            }
            root
        }
    };

    let mut ctx = IrContext::new();
    let mut lowerer = IrLowerer::new(&files, &mut ctx, root_module.name);
    let mut diags = DiagnosticManager::new(&files);
    lowerer
        .lower(&root_module)
        .map_err(|e| diags.emit(e))
        .unwrap_or_else(|()| std::process::exit(-1));

    drop(lowerer);
    let llvm = Context::create();
    let codegen = LLVMCodeGenerator::new(&files, &mut ctx, &llvm);
    codegen.gen(opts);
}

fn handle_parse_error<T>(res: Result<T, ParseError>, files: &Files, file: FileId) -> T {
    res.unwrap_or_else(|e| {
        let mut diags = DiagnosticManager::new(files);
        let diag = Diagnostic::error()
            .with_message(e.error.to_string())
            .with_notes(
                e.backtrace
                    .iter()
                    .map(|trace| format!("in {}", trace))
                    .collect(),
            );

        diags.emit(if let Some(span) = e.highlighted_span {
            diag.with_labels(vec![Label::primary(file, span)])
        } else {
            diag
        });

        std::process::exit(-1);
    })
}

/// Recursively submit all child source files and modules of a directory to the given parser
fn parse_dir<'src>(
    name: String,
    items: Vec<InputItem>,
    files: &'src Files,
    parser: &mut Parser<'src>,
) -> ParsedModule {
    let mut root = ParsedModule::new(Symbol::from(&name));

    for item in items {
        match item {
            InputItem::File(f) => {
                let src = files.get(f).text.as_str();
                parser.set_text(src);
                handle_parse_error(parser.parse_to(&mut root, f), &files, f);
            }
            InputItem::Dir(name, items) => {
                let child = parse_dir(name.clone(), items, files, parser);
                root.children.push(child);
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
            InputItem::Dir(
                input.file_name().unwrap().to_string_lossy().into_owned(),
                items,
            )
        }
        false => {
            let id = files.add(CompiledFile::open(input).expect("failed to open a file"));
            InputItem::File(id)
        }
    }
}
