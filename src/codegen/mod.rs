use codespan_reporting::diagnostic::Diagnostic;

use crate::util::files::FileId;

pub mod ir;
pub mod llvm;
pub mod lower;


pub type CompilerRes<T> = Result<T, Diagnostic<FileId>>;
