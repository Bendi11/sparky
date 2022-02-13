# Binary Expressions

```ebnf
<binexpr> ::= <expr> <op> <expr>
```

---

## Operator Precedence
In order to reduce confusion and simplify parsing, operators have no precendence and the
order of operations in spark is left to right

## Valid Operators for Each Data Type

> `(i|u)xx` \<op\> `(i|u)xx`: +, -, *, /, %, <<, >>, &, |, ^
> `fxx` \<op\> `fxx`: +, -, *, /, %
> `fxx` \<op\> `uxx`: <<, >>
> 

