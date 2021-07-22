//! The `code` module contains submodules handling linking and LLVM code generation
pub mod llvm;
pub mod linker;
pub use llvm::Compiler;