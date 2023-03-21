/// Postprocessing phases occur after the preprocessing and type-checking phases.
use crate::ast::typed::typed_node::TypedNode;

mod pattern;

pub mod pattern_phase;

pub trait PostprocessPhase {
    fn transform(&self, root: &TypedNode) -> TypedNode;
}
