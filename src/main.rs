pub mod ast;
pub mod code;
pub mod lex;
pub mod parser;
pub mod types;
use std::io::Write as IoWrite;
use std::{panic::PanicInfo, path::PathBuf, str::FromStr};

use bumpalo::Bump;
use clap::{App, Arg};
use inkwell::context::Context;
use simplelog::{Color, CombinedLogger, TermLogger, WriteLogger};
use termcolor::{ColorSpec, StandardStream, WriteColor};
pub use types::Type;

use crate::code::{linker, Compiler};

/// Optimization level that can be given as a command line argument
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptLvl {
    Debug,
    Medium,
    Aggressive,
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

    /// Ignore LLVM module verification step
    ignore_checks: bool,

    /// Additional linker arguments supplied by the user
    linker_args: Vec<String>,
}

/// Handle compiler errors in a cleaner way
pub fn panic_handler(p: &PanicInfo) {
    let mut stdout = StandardStream::stderr(match atty::is(atty::Stream::Stderr) {
        true => termcolor::ColorChoice::Auto,
        false => termcolor::ColorChoice::Never,
    });
    let _ = stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true));
    writeln!(&mut stdout, "A fatal error occurred when compiling").ok();
    stdout.reset().ok();
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

/// Setup the logging handler with [`fern`]
fn setup_logger(
    verbosity: log::LevelFilter,
    colorchoice: termcolor::ColorChoice,
) -> Result<(), log::SetLoggerError> {
    let logger = TermLogger::new(
        log::LevelFilter::Warn,
        simplelog::ConfigBuilder::new()
            .set_level_color(log::Level::Error, Some(Color::Red))
            .set_level_color(log::Level::Warn, Some(Color::Yellow))
            .set_time_level(log::LevelFilter::Off)
            .set_location_level(log::LevelFilter::Off)
            .build(),
        simplelog::TerminalMode::Stderr,
        colorchoice,
    );

    let mut loggers = vec![logger as Box<dyn simplelog::SharedLogger>];
    if verbosity != log::LevelFilter::Warn {
        loggers.push(WriteLogger::new(
            verbosity,
            simplelog::ConfigBuilder::new().build(),
            std::fs::OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open("sparkc.log")
                .unwrap(),
        ) as Box<dyn simplelog::SharedLogger>);
    }

    CombinedLogger::init(loggers)
}

fn main() {
    std::panic::set_hook(Box::new(panic_handler));

    let app = App::new("sparkc")
        .about("Compiler for the Spark programming language utilizing LLVM as a backend")
        .version(clap::crate_version!())
        .setting(clap::AppSettings::ColoredHelp)
        .author("Ben Liebmann <bkliebmann@gmail.com>")
        .long_about(
"Compiler for the Spark programming language, supporting LLVM as a code generation backend.
 Supports compiling libraries, executable files, outputting LLVM IR, and compiling to object files.
")
        .version(clap::crate_version!())
        .arg(Arg::new("input-files")
            .short('i')
            .long("input-files")
            .multiple(true)
            .min_values(1)
            .takes_value(true)
            .about("Spark source code files to compile")
            .long_about("A list of input file paths to spark (.sprk) source files to compile")
            .validator(|s| match PathBuf::from(s) {
                p if p.exists() => Ok(()),
                p => Err(format!("Input file {} does not exist!", p.display()))
            })
            .required_unless_present("input-dir")
            .conflicts_with("input-dir")
        )
        .arg(Arg::new("input-dir")
            .short('d')
            .long("input-dir")
            .about("Select a directory or list of directories containing all spark source files to be parsed")
            .takes_value(true)
            .multiple(true)
            .allow_hyphen_values(true)
            .number_of_values(1)
            .validator(|c| match std::path::Path::new(&c).exists() {
                true => Ok(()),
                false => Err(format!("The directory at {} does not exist", c))
            })
        )
        .arg(Arg::new("library")
            .short('l')
            .multiple(true)
            .takes_value(true)
            .long("library")
            .about("Specify the path to one library file (.lib) that will be linked to produce the final executable file")
            .long_about("Specify a library file to link to when creating the executable file. This file will be searched by the native linker and if not found will cause linking errors")
        )
        .arg(Arg::new("optimization")
            .short('z')
            .long("optimization-level")
            .about("Set the optimization level of the finished executable file")
            .long_about("Set the level of optimization to apply to the executable file. This controlls how many LLVM optimization passes are applied to the intermediate representation")
            .default_value("0")
            .possible_values(&["0", "1", "2", "debug", "release"])
            .multiple(false)
            .max_values(1)
            .takes_value(true)
        )
        .arg(Arg::new("output-file")
            .short('o')
            .long("output-file")
            .about("Specify the path to save the completed output file to (excluding file extension)")
            .max_values(1)
            .multiple(false)
            .takes_value(true)
            .required(true)
        )
        .arg(Arg::new("output-type")
            .short('f')
            .long("output-format")
            .about("Specify the type of output file to compile to")
            .long_about("Specify the type of file to produce when compiling")
            .takes_value(true)
            .possible_values(&["exec", "lib", "ll", "asm", "obj", "ir"])
            .default_value("exec")
            .required(true)
            .max_values(1)
            .takes_value(true)
        )
        .arg(Arg::new("verbose")
            .short('v')
            .long("verbose")
            .about("Enable verbose logs for the compiler (useful when debugging an issue when compiling)")
            .takes_value(false)
        )
        .arg(Arg::new("unchecked")
            .about("If enabled, LLVM module checks will be reduced to warnings instead of fatal errors")
            .takes_value(false)
            .short('u')
            .long("unchecked")
        )
        .arg(Arg::new("linker-args")
            .about("Pass additional arguments to the platform-specific linker")
            .long("linker-arg")
            .number_of_values(1)
            .multiple_occurrences(true)
            .allow_hyphen_values(true)
            .takes_value(true)
        )
        .arg(Arg::new("no-color")
            .about("If enabled, no color is used when writing messages to stdout / stderr")
            .takes_value(false)
            .long("nc")
        );
    let args = app.get_matches(); //Get argument matches from environment args

    let color = match args.is_present("no-color") {
        true => termcolor::ColorChoice::Never,
        false => match atty::is(atty::Stream::Stderr) {
            true => termcolor::ColorChoice::Auto,
            false => termcolor::ColorChoice::Never,
        },
    };

    match args.is_present("verbose") {
        true => setup_logger(log::LevelFilter::Trace, color),
        false => setup_logger(log::LevelFilter::Warn, color),
    }
    .unwrap();

    //Get the given compilation options
    let opts = CompileOpts {
        libraries: match args.values_of("library") {
            Some(l) => l.map(|s| s.to_owned()).collect(),
            None => Vec::new(),
        },
        opt_lvl: args.value_of("optimization").unwrap().parse().unwrap(),
        output_ty: args.value_of("output-type").unwrap().parse().unwrap(),
        out_file: PathBuf::from(args.value_of("output-file").unwrap()),
        ignore_checks: args.is_present("unchecked"),
        linker_args: args
            .values_of("linker-args")
            .map(|s| s.map(|val| val.to_owned()).collect::<Vec<_>>())
            .unwrap_or_default(),
    };

    //Get a list of files to parse into an AST
    let input_files: Vec<String> = match args.values_of("input-files") {
        Some(v) => v.map(|s| s.to_owned()).collect(),
        None => {
            let dirs = args.values_of("input-dir").unwrap();
            let mut list = Vec::new();

            /// Recursively search a directory for .sprk files
            fn read_dir(path: &str, list: &mut Vec<String>) {
                std::fs::read_dir(path)
                    .unwrap()
                    .fold(list, |list, s| match s {
                        Ok(entry) if entry.path().extension().unwrap_or_default() == "sprk" => {
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

            for dir in dirs {
                read_dir(dir, &mut list);
            }

            list
        }
    };

    let mut ast = vec![];

    //Parse all files into an AST
    let ctx = Context::create();

    //Parse every file
    for filename in input_files.iter() {
        let mut file = std::io::BufReader::new(std::fs::File::open(filename).unwrap()); //We can unwrap the file opening because all file names are validated by clap as being existing files
        let lexer = lex::Lexer::from_reader(&mut file, filename.clone());
        ast.extend(
            parser::Parser::new(lexer)
                .parse()
                .unwrap_or_else(|e| panic!("Error when parsing file {}: {}", filename, e)),
        ); //Parse the file and add it to the AST
    }

    let arena = Bump::new();
    let compiler = Compiler::new(&ctx, &arena);
    let name = opts.out_file.clone();

    #[cfg(target_os = "windows")]
    let linker = linker::WinLink::default();

    #[cfg(target_os = "linux")]
    let linker = linker::LdLink::default();

    match compiler.compile(ast, opts, linker) {
        Ok(()) => println!("{} compiled successfully!", name.display()),
        Err(count) => eprintln!(
            "{}: Failed to compile due to {} {}",
            name.display(),
            count,
            match count > 1 {
                true => "errors",
                false => "error",
            }
        ),
    }
}
