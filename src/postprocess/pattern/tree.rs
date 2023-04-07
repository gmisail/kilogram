use super::{clause::Clause, Pattern};

/// Represents the following expression:
///
///     match <constructor> {
///         C(a_0, ..., a_n) => <matching_group>,
///         _ => <remaining_group>
///     }
///
pub struct MatchTree {
    constructor: Pattern,
    matching_group: Vec<Clause>,
    remaining_group: Vec<Clause>,
}
