use sparky_arena::Arena;

sparky_arena::new_arena_key!{
    /// Test
    pub struct FileId;
}

/// A collection of parsed data with 
#[derive(Clone, Debug)]
pub struct Files {
    arena: Arena<OpenFile>,
}



#[derive(Clone, Debug)]
enum OpenFile {
    
}
