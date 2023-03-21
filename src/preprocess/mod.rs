/// Preprocessing phases occur before the type-checking phase.
use crate::ast::untyped::untyped_node::UntypedNode;

pub trait PreprocessPhase {
    fn transform(&self, root: &UntypedNode) -> UntypedNode;
}
