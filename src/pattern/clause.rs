use crate::pattern::Pattern;

/// Represents an individual test condition. For
/// instance: 
///     a_1 is Some(a_2)
pub struct Test {
    bound_variable: String,
    pattern: Pattern,
    value: i32
}

type Clause = Vec<Test>;
