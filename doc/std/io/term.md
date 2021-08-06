# std::io::term 
The `std::io::term` module should provide a cross-platform API for 
- Reading from stdin
- Writing to stdout and stderr

## Structure Types
- `Stream`: Contains all state needed to write to / read from a stream like stdout, stderr, etc.

## Functions 
```rust
fun stdout() Stream
```
> Get a `Stream` structure holding a file descriptor for stdout on linux and a HANDLE to stdout for windows
----
```rust
fun stderr() Stream
```
> Get a `Stream` structure holding a file descriptor for stderr on linux and a HANDLE to stderr for windows
----
```rust
fun stdin() Stream
```
> Get a `Stream` structure holding a file descriptor for stdin on linux and a HANDLE to stdin for windows
----
```rust
fun write(Stream ostream, u8 ptr buf, usize len) isize
```
> Write to a `Stream` object, returning either a positive value for number of bytes written or a negative error
----
```rust
fun read(Stream istream, u8 ptr buf, usize len) isize
```
> Read from a `Stream` object, returning either a positive value for number of bytes read or a negative error
----
```rust
fun puts(u8 ptr str) isize
```
> Write a NULL-terminated string to stdout, returning the amount of bytes written or a negative error code