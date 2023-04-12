use sparky_arena::Arena;

sparky_arena::new_arena_key!{FileId}

#[derive(Clone, Debug)]
pub struct Files {
    arena: Arena<File>,
}

#[derive(Clone, Debug)]
pub enum File {
    Memory(String),
}
