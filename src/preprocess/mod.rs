/// Preprocessing phases occur before the type-checking phase.
use crate::typed::typed_node::TypedNode;

pub mod pattern;

pub trait PreprocessPhase {
    fn transform(&self, root: &UntypedNode) -> UntypedNode;
}
