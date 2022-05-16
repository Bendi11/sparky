# Compiler Structure
The compiler is organized into separate stages, each lowering the program until reaching LLVM IR

#1: Parsing
 - Read all input files, categorized into modules based on what directory they are in
  - Initialize a Files structure to associate unique file IDs with file path and text data for error messages
  - From the input file tree, build an initial root ParsedModule containing all submodules that correspond to subdirectories
  in the file tree
 - Parse every input file's definitions into the ParsedModule that corresponds to the directory containing the file
  - Includes type definitions, function declarations / definitions, constants, and import definitions
  - ParsedModules contain a Vec of ParsedDefs which contain
   - Function definitions / declarations
   - Import definitions
   - Type definitions
  - Function definitions contain a list of Stmts in the order that they appear
   - Stmts contain expressions, function calls, etc.


#2: Semantic Analysis - Lowering to IR
 - Create an empty symbol table used to match identifiers with user-defined functions, types, variables, etc.
 - Walk the generated AST to populate symbol table forward declarations for all types
 - Walk the AST to populate symbol table type definitions to IRTypes and function declarations to IRFuns
 - Walk the AST to lower the bodies of all defined functions to IRStmts
  - Resolve all user-defined data using the symbol table for the current module
  - Ensure validity of function body
   - Type check all operators and de-sugar implicit casts like when adding a number to a pointer
   - Ensure all assignment operations assign the same type
   - Match expressions that `phi` a value must always return the same type
   - Return types must match the defining function
  - Break the tree structure into blocks and (conditional) jumps
  - De sugar phi expressions to a phi value allocation and assignment
  - De sugar structure field accesses to indexed accesses

#3 Codegen
 - Walk the generated IR 
