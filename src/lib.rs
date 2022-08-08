use std::path::PathBuf;

use internment::LocalIntern;

pub mod arena;
pub mod ast;
pub mod error;
pub mod ir;
pub mod llvm;
pub mod parse;
pub mod util;

pub type Symbol = LocalIntern<String>;

/// Enumeration labelling all output formats that the compiler can produce
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OutputFileType {
    Assembly,
    Object,
    LLVMIR,
}

/// Enumeration representing all supported optimization profiles for the
/// generated output
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OutputOptimizationLevel {
    Release = 3,
    Medium = 2,
    Size = 1,
    Debug = 0,
}

/// Structure with all configurable properties of code generation
#[derive(Clone, Debug)]
pub struct CompileOpts {
    /// The output file to produce
    pub out_type: OutputFileType,
    /// Path to an output file
    pub out_file: PathBuf,
    /// How much the output should be optimized
    pub opt_lvl: OutputOptimizationLevel,
    /// Generate position independent code
    pub pic: bool,
    /// If symbols should be stripped from the output
    pub stripped: bool,
}
