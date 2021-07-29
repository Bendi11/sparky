# Spark programming language
------
A small, feature-light compiled general purpose programming language utilizing LLVM as a compiler backend. Made with the eventual goal of
a well-rounded standard library, build system, and self hosting compiler. 

## Examples
A simple hello world program utilizing the standard library is below:
```rust
use std::io

fun main() i32 {
  io::puts("Hello World!!!");
  ret 0;
}
```

Further examples can be found in the [wiki](https://github.com/Bendi11/spark/wiki)
