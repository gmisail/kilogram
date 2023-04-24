/// Preprocessing phases occur before the type-checking phase.
///
use crate::ast::untyped::untyped_node::UntypedNode;

use self::generic::phase::GenericPhase;

mod generic;

pub trait PreprocessPhase {
    fn transform(&mut self, root: &UntypedNode) -> UntypedNode;
}

pub fn apply_all(root: &UntypedNode) -> UntypedNode {
    GenericPhase::new().transform(root)
}
