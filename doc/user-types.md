 # User-Defined Types

A programmer can define their own types with associated methods using the `type` keyword

```
type MyType = { i32 a, slice[u8] b };

type Interface(T) = {
    fun(): *T inst,
}

MyType = #{
    new = fun(): MyType {
        return #MyType {
            a = 3,
            b = slice.of("test")
        };
    }
};

MyType[Interface(MyType)] = #{
    inst = fun(): *MyType {
        return std.null;
    }
}
```
