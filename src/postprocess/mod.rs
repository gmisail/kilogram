/// Postprocessing phases occur after the preprocessing and type-checking phases.
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::typed::{data_type::DataType, typed_node::TypedNode};

use self::pattern_phase::PatternPhase;

mod pattern;
mod pattern_phase;

pub trait PostprocessPhase {
    fn transform(&self, root: &TypedNode) -> TypedNode;
}

pub fn apply_all(root: &TypedNode, enums: &HashMap<String, Rc<DataType>>) -> TypedNode {
    // For now, only apply the patter compiler. In the future, we'd create a pipeline of
    // operations such that each of the operations pipe its result to the next phase.
    PatternPhase::new(enums).transform(root)
}
