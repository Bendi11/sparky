# std::alloc
The `std::alloc` module should provide a cross-platform API for allocating, de-allocating, and re-allocating
memory on the heap.

# Functions
```rust
fun malloc(usize nbytes) u8 ptr
```
> Allocate a certain number of bytes on the heap, returning a pointer to the allocated
> memory, or NULL if allocation failed
----
```rust
fun free(u8 ptr mem) void 
```
> Free memory previously allocated with `malloc`