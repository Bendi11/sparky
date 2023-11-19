pub mod sir;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntegerWidth {
    Eight,
    Sixteen,
    ThirtyTwo,
    SixtyFour,
}

/// An integral integer type with a given bit width and sign flag
#[derive(Clone, Copy, Debug,)]
pub struct IntegerType {
    pub sign: bool,
    pub width: IntegerWidth,
}

pub struct TypeId;
