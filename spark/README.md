# spark 
Syntax highlighing for the `spark` programming language. The language was a summer project for fun, its grammar can be found here: 

```bnf
(*A program is made up of declarations*)
<program> ::= <decl>*
(*A declaration can be either a type definition or function definition*)
<decl> ::= "fun" <attr>* <ident> "(" (<typename> <ident> ",")* (<typename> <ident>)? ")" <typename> <body>?
		| <structdecl>
        | <uniondecl>
        | "type" <typename> <ident>

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