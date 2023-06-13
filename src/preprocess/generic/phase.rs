use super::passes;
use crate::ast::untyped::untyped_node::UntypedNode;

use crate::preprocess::PreprocessPhase;

pub struct GenericPhase {}

impl PreprocessPhase for GenericPhase {
    fn transform(&mut self, root: &UntypedNode) -> UntypedNode {
        passes::apply_passes(root)
    }
}
