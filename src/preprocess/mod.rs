/// Preprocessing phases occur before the type-checking phase.
///
use crate::ast::untyped::untyped_node::UntypedNode;

use self::generic_phase::GenericPhase;

mod generic;
mod generic_phase;

pub trait PreprocessPhase {
    fn transform(&mut self, root: &UntypedNode) -> UntypedNode;
}

pub fn apply_all(root: &UntypedNode) -> UntypedNode {
    GenericPhase::new().transform(root)
}
