use tracing::info;

/// Preprocessing phases occur before the type-checking phase.
///
use crate::ast::untyped::untyped_node::UntypedNode;

use self::generic::phase::GenericPhase;

mod generic;

pub trait PreprocessPhase {
    fn transform(&mut self, root: &UntypedNode) -> UntypedNode;
}

#[tracing::instrument]
pub fn apply_all(root: &UntypedNode) -> UntypedNode {
    GenericPhase {}.transform(root)
}
