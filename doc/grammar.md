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
    struct Structure s = struct Structure {
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


## Grammar in Extended Backus-Naur Form
```ebnf
(*A program is made of declarations only*)
program = { declaration } ;

declaration = function declaration 
            | struct declaration 
            | union declaration ;

function declaration = 'fun' , { attribute } , identifier , '(' , { type , [ identifier ] , ',' } , ')' , type , [ body ] ;

(*Struct type declaration that declares data type that the struct holds*)
struct declaration = 'struct' , identifier , '{' , { type , ident , ',' } , '}' ;


union declaration = 'union' , ident , '{' , { type , ident , ',' } , '}' ;

identifier = character , [{ character | number }] ;
character = ?Any ASCII alphabetic character? | '_' ;
number = ?Any digit 0-9? ;

type = int type
    | struct type
    | union type 
    | type , 'ptr' 
    | bool type 
    ;

bool type = 'bool' ;
int type = ( 'i' | 'u' ) , ( '8' | '16' | '32' | '64' ) ;
struct type = 'struct' , ident ;
union type = 'union' , ident ;

(*A body is a list of expressions enclosed in curly braces*)
body = '{' , { complete expression } , '}' ;

(*Primary expressions are expressions that must be parsed first, and then binary operators can combine them into binary expressions*)
primary expression = variable declaration 
                    | literal 
                    | function call 
                    | unary expression 
                    | variable access
                    ;
unary expression = operator , expression ;

function call = ident , '(' { expression , ',' } , ')' 
              | variable access , '.' , function call (*Calling an associated function of a type using a value*)
              ;

variable access = ident 
                | ident , '.' , ident (*Accessing a field of a struct or union*)
                ;

(*A typename followed by an identifier is a variable declaration*)
variable declaration = type , { attribute } , ident ;

literal = string literal 
        | int literal
        | struct literal
        | union literal 
        | bool literal 
        ;

bool literal = 'true' | 'false' ;
string literal = '"' , ?Any text excluding '"'? , '"' ;
int literal = { number } , [ int type ] ; 
struct literal = struct type , '{' { ident , '=' , expression , ',' } , '}' ;
union literal = union type , '{' ident '=' , expression , '}' ;

attribute = 'ext' | 'const' ;

binary expression = expression , operator , expression ;
operator = '+' 
        | '/' 
        | '*' 
        | '-'
        | '&' 
        | '^' 
        | '%'
        | '|'
        | '||'
        | '&&'
        | '='
        | '=='
        ;
    
expression = binary expression 
            | primary expression
            | '(' , expression , ')' 
            | 'ret' , expression 
            | if expression
            | while expression
            ;

if expression = 'if' , expression , body , [ 'else' , body ] ;
while expression = 'while' , expression , body ;

(*Complete semicolon terminated expression*)
complete expression = expression , ';' ;
```