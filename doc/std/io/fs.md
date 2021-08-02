# std::io::fs
The `fs` module should provide an API to interact with files in a cross-platform way. This must include
- Opening files with permissions
- Closing files
- Writing to files
- Reading from files
- Accessing file metadata that is availible in a cross-platform way

## Structure types
- `File`: Must contain any file descriptors or handles needed to access the file using platform-specific APIs
- `Opts`: Must contain permission data needed for opening files

## Typedefs
- `type u8 opt_flag`: Flag used for adding permissions to the `Opts` struct

## Constants
- `opt_flag CREATE_NEW = 0`: Create a new file if no file exists when opening a file
- `opt_flag ALWAYS_CREATE = 1`: Create a new file always, and if the file already exists return an error
- `opt_flag READ = 2`: Open a file in read mode
- `opt_flag WRITE = 3`: Allow writing to an opened / created file
- `opt_flag TRUNCATE_EXISTING = 4`: Truncate an existing file if it already exists
- `opt_flag APPEND_EXISTING = 5`: Append to a file if it already exists

## Functions
----

```rust
fun write(File ptr file, u8 ptr buf, usize len) isize
```
Write a specified amount of bytes from a byte buffer to the given `File` object, returning an error indicated with a negative return value,
or the amount of bytes written to the file.
----
```rust
fun read(File ptr file, u8 ptr buf, usize len) isize
```
Read the specified amount of bytes from the file into the specified byte array, returning a negative error code or the amount of bytes read from the file.
----
```rust
fun open(u8 ptr filename, Opts openopts) File ptr
```
Open or create a file from the specified path, using the specified file flags for permissions and behavior if the 
file already exists / doesn't exist. Returns `NULL` on error or an allocated file struct on success.
----
```rust
fun close(File ptr file) bool
```
Close the specified file, returning `true` if the file was closed successfully or `false` if an error was encountered
while closing the file.
----
```rust
fun opts_default() Opts
```
Return an `Opts` struct with default values per-platform, but with the flags:
- `CREATE_NEW`
- `READ`
- `WRITE`
- `TRUNCATE_EXISTING`
----
```rust
fun add_opt_flag(Opts ptr opts, opt_flag flag) void
```
Add a given flag to the `Opts` struct
----
```rust
fun rem_opt_flag(Opts ptr opts, opt_flag flag) void
```
Remove the given flag from the `Opts` struct