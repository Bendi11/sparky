# Grammar
The grammar of Spark is roughly standard curly braces semicolon terminated, like C or Rust. It features 
- Unions of values
- Structs that hold a collection of data
- Functions with the fun keyword

Examples 
```
struct Structure {
    i32 num,
    u8 ptr string,
}

fun main() i32 {
    let Structure s = struct Structure {
        num = 10,
        string = "testing",
    };
}
```

## Keywords
The keywords and their purposes are: 
- `fun`: Declare a function
- `struct`: Declare a structure or a structure typename
- `union`: Declare a union or a union typename
- `ret`: Return from a function
- `if`: Control flow checking conditions
- `else`: Control flow following a failed if check
- `while`: Looping with conditional checking
- `const`: Attributes for variables and functions
- `ext`: Mark a function or global variable as externally defined
- `ptr`: Express a pointer type
- `let`: Declare a variable 
- `ns`: Declare a namespace
- `use`: Import a namespace into another namespace

## Operators
The operators and their purposes: 
- `+`: Add two expressions
- `-`: Subtract the right hand side expression from the left hand side expression
- `*`: Multiply two expressions or dereference a pointer 
- `/`: Divide the left hand by the right hand expresion
- `%`: Perform modulus of the left and right hand side expressions
- `=`: Assign the right hand side expression to the left hand side
- `==`: Check if the left hand side expression is equivalent to the right hand side
- `^`: Exclusive OR the right and left hand side expressions
- `&`: AND the left and right hand expressions
- `|`: OR the right and left hand expressions
- `&&`: Ensure that both the left and right hand expressions are true
- `||`: Check if either the left or right hand expression is true
- `>`: Check if the left hand side is greater than the right hand side
- `<`: Check if the left hand side is less than the right hand side
- `<=`: Check if the left hand side if less than or equal to the right hand side
- `>=`: Check if the left hand side is greater than or equal to the right hand side


## Grammar in Backus-Naur Form
```ebnf 
(*A program is made up of declarations*)
<program> ::= <decl>*
(*A declaration can be either a type definition or function definition*)
<decl> ::= "fun" <attr>* <ident> "(" (<typename> <ident> ",")* (<typename> <ident>)? ")" <typename> <body>?
		| <structdecl>
        | <uniondecl>
        | "type" <typename> <ident>
        | <nsdecl>
        | <usingdecl>

<nsdecl> ::= "ns" <ident> "{" <program> "}"
<usingdecl> ::= "use" <ident>

(*If a struct declaration has no definition, it is parsed as an opaque type*)
<structdecl> ::= "struct" <ident> ( "{" (<typename> <ident> ",")* (<typename> <ident>)? "}" )?
(*Union types have no reason to ever be opaque types so always must have a definition*)
<uniondecl> ::= "union" <ident> "{" (<typename> <ident> ",")* (<typename> <ident>)? "}"
        
<attr> ::= "ext" 
		| "const"
        | "static"
        
(*Top level expressions are the ones that must be parsed in functions first, expressions are parsed in them*)
<topexpr> ::= "let" <typename> <attr>* <ident> ("=" <expr>)?
	| <funcall>
    | "if" <expr> <body> ("else" <body>)?
    | "while" <expr> <body>


(*The body of a function or if statement*)
<body> ::= "{" (<topexpr> ";")* "}"

<expr> ::= <literal> | <prefix> | <binary> | <cast> | <unary>

<binary> ::= <expr> <operator> <expr>
<unary> ::= <operator> <expr>
<cast>   ::= "{"<typename>"}"<expr>

<literal> ::= <numberliteral> | "\"" <ident> "\"" | <structliteral>
<structliteral> ::= "struct" <ident> "{" (<ident> "=" <expr> ",")* (<ident> "=" <expr>)? } 
<numberliteral> ::= <digit>+ <inttype>? | "true" | "false"

<typename> ::= <inttype> | <ident> 
<inttype> ::= ("i" | "u") ("8" | "16" | "32" | "64") | "bool"
<var> ::= <ident> | <prefix> "." <ident> 

(*Prefix expressions are expressions that can come before a member access with the "." operator*)
<prefix> ::= <var> | <funcall> | "(" <expr> ")"

<funcall> ::= <ident> "(" <args> ")" 
<args> ::= (<expr> ",")* <expr>?

<operator> ::= "+" 
	| "-"
    | "*"
    | "/"
    | "%"
    | "="
    | "=="
    | "^"
    | "&"
    | "|"
    | "&&"
    | "||" 
    | ">" 
    | "<"
    | "<="
    | "<="
    
<ident> ::= <letter>+ (<letter> | <digit> | "_")* 
    
<letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G"
       | "H" | "I" | "J" | "K" | "L" | "M" | "N"
       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
       | "c" | "d" | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n" | "o" | "p"
       | "q" | "r" | "s" | "t" | "u" | "v" | "w"
       | "x" | "y" | "z" 
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 


```
