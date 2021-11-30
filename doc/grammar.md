# Spark Grammar

### Type Definitions
* Enum
 > type enumerated = i32 | bool
* Structure
 > type structure = { a: i32, b: bool }
* Tuple
 > type tuple_structure = ( i32, bool )

### Function Declaration / Definition
* Declaration
 > fun declared i32, bool
* Definition
 > fun defined arg: i32, otherarg: bool { ... }

 > fun defined -> bool { true }