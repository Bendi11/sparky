pub mod ast;
pub mod code;
pub mod lex;
pub mod types;
pub mod parser;
use std::{panic::PanicInfo, path::PathBuf, process::{Command, Stdio}, str::FromStr};

use clap::{App, AppSettings, Arg};
use console::style;
use inkwell::{context::Context};
pub use types::Type;

use crate::code::Compiler;


/// Optimization level that can be given as a command line argument
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLvl {
    Aggressive,
    Medium,
    Debug,
}

impl FromStr for OptLvl {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" | "debug" => Ok(Self::Debug),
            "1" => Ok(Self::Medium),
            "2" | "release" => Ok(Self::Aggressive),
            _ => Err(()),
        }
    }
}

/// Output file format for compiler
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutFormat {
    Obj,
    Exe,
    IR,
    Lib,
    Asm,
}

impl FromStr for OutFormat {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "exec" => Ok(Self::Exe),
            "lib" => Ok(Self::Lib),
            "ll" | "ir" => Ok(Self::IR),
            "asm" => Ok(Self::Asm),
            "obj" => Ok(Self::Obj),
            _ => Err(()),
        }
    }
}

/// Compiler options that are changed with command line switches and arguments
#[derive(Debug, Clone)]
pub struct CompileOpts {
    /// A list of libraries to link to when linking an exe
    libraries: Vec<String>,

    /// Optimization level
    opt_lvl: OptLvl,

    /// The output file format
    output_ty: OutFormat,

    /// The output file
    out_file: PathBuf,
}

/// Handle compiler errors in a cleaner way
pub fn panic_handler(p: &PanicInfo) {
    eprintln!(
        "{}",
        style("A fatal error occurred when compiling")
            .bright()
            .red()
            .bold()
    );
    match p.location() {
        Some(loc) => eprintln!("At {}", loc),
        None => eprintln!("In an unknown location"),
    }
    match (
        p.payload().downcast_ref::<&str>(),
        p.payload().downcast_ref::<String>(),
    ) {
        (Some(s), _) => eprintln!("Error: {}", s),
        (_, Some(s)) => eprintln!("Error: {}", s),
        (None, None) => eprintln!("With a non-displayable error"),
    }
    std::process::exit(-1);
}


fn main() {
    std::panic::set_hook(Box::new(panic_handler));

    let app = App::new("sparkc")
        .about("Compiler for the Spark programming language utilizing LLVM as a backend")
        .author("Bendi11 <bkliebmann@gmail.com>")
        .long_about(
"Compiler for the Spark programming language, supporting LLVM as a code generation backend.
 Supports compiling libraries, executable files, outputting LLVM IR, and compiling to object files.
")
        .setting(AppSettings::ColorAuto)
        .setting(AppSettings::ColoredHelp)
        .version(clap::crate_version!())
        .arg(Arg::with_name("input-files")
            .short("i")
            .long("input-files")
            .multiple(true)
            .min_values(1)
            .takes_value(true)
            .help("Spark source code files to compile")
            .long_help("A list of input file paths to spark (.sprk) source files to compile")
            .validator(|s| match PathBuf::from(s) {
                p if p.exists() => Ok(()),
                p => Err(format!("Input file {} does not exist!", p.display()))
            })
            .required_unless("input-dir")
            .conflicts_with("input-dir")
        )
        .arg(Arg::with_name("library")
            .short("l")
            .multiple(true)
            .takes_value(true)
            .long("library")
            .help("Specify the path to one library file (.lib) that will be linked to produce the final executable file")
            .long_help("Specify a library file to link to when creating the executable file. This file will be searched by the native linker and if not found will cause linking errors")
        )
        .arg(Arg::with_name("optimization")
            .short("z")
            .long("optimization-level")
            .help("Set the optimization level of the finished executable file")
            .long_help("Set the level of optimization to apply to the executable file. This controlls how many LLVM optimization passes are applied to the intermediate representation")
            .default_value("0")
            .possible_values(&["0", "1", "2", "debug", "release"])
            .multiple(false)
            .max_values(1)
            .takes_value(true)
        )
        .arg(Arg::with_name("output-file")
            .short("o")
            .long("output-file")
            .help("Specify the path to save the completed output file to")
            .max_values(1)
            .multiple(false)
            .takes_value(true)
            .required(true)
        )
        .arg(Arg::with_name("output-type")
            .short("f")
            .long("output-format")
            .help("Specify the type of output file to compile to")
            .long_help("Specify the type of file to produce when compiling")
            .takes_value(true)
            .possible_values(&["exec", "lib", "ll", "asm", "obj", "ir"])
            .default_value("exec")
            .required(true)
            .max_values(1)
            .takes_value(true)
        );
    let args = app.get_matches(); //Get argument matches from environment args

    //Get the given compilation options
    let opts = CompileOpts {
        libraries: match args.values_of("library") {
            Some(l) => l.map(|s| s.to_owned()).collect(),
            None => Vec::new(),
        },
        opt_lvl: args.value_of("optimization").unwrap().parse().unwrap(),
        output_ty: args.value_of("output-type").unwrap().parse().unwrap(),
        out_file: PathBuf::from(args.value_of("output-file").unwrap()),
    };

    //Get a list of files to parse into an AST
    let input_files: Vec<String> = match args.values_of("input-files") {
        Some(v) => v.map(|s| s.to_owned()).collect(),
        None => unreachable!(),
        /*None => {
            let dir = args.value_of("input-dir").unwrap();
            let mut list = Vec::new();

            /// Recursively search a directory for .sprk files
            fn read_dir(path: &str, list: &mut Vec<String>) {
                std::fs::read_dir(path)
                    .unwrap()
                    .fold(list, |list, s| match s {
                        Ok(entry)
                            if entry.path().extension().unwrap_or(OsStr::new("")) == "sprk" =>
                        {
                            list.push(entry.path().to_str().unwrap().to_owned());
                            list
                        }
                        Ok(entry) if entry.file_type().unwrap().is_dir() => {
                            read_dir(entry.path().to_str().unwrap(), list);
                            list
                        }
                        _ => list,
                    });
            }

            read_dir(dir, &mut list);

            
            list
        }*/
    };

    //Parse all files into an AST
    let mut modules = Vec::new();
    let ctx = Context::create();
    

    //Parse every file
    for file in input_files.iter() {
        let code = code::Compiler::new(&ctx);
        let mut opts = opts.clone();
        opts.output_ty = OutFormat::Obj;
        opts.out_file = PathBuf::from(file.clone() + ".obj");
        opts.libraries = vec![];

        let mut file = std::io::BufReader::new(std::fs::File::open(file).unwrap()); //We can unwrap the file opening because all file names are validated by clap as being existing files
        let lexer = lex::Lexer::from_reader(&mut file);
        let ast = 
            parse::ProgramParser::new()
                .parse(lexer)
                .unwrap_or_else(|e| panic!("Error when parsing: {}", e));//Parse the file
        code.compile(ast, opts.clone()); //Compile to an obj
        modules.push(opts.out_file);
    }    

    const LINKER: &str = "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.28.29910\\bin\\Hostx64\\x64\\link.exe";


    let out = format!("/OUT:{}", opts.out_file.display());
    let mut args = vec![
        "/ENTRY:main",
        "/SUBSYSTEM:console",
        "/NOLOGO",
        out.as_str(),
    ];
    args.extend(modules.iter().map(|s| s.to_str().unwrap()));
    args.extend(opts.libraries.iter().map(|s| s.as_str())); //Add all linked libraries

    let cmd = Command::new(PathBuf::from(LINKER))
        .args(args)
        .stderr(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap(); //Link the file into a library
    println!(
        "{}",
        String::from_utf8(cmd.wait_with_output().unwrap().stdout).unwrap()
    );
}
