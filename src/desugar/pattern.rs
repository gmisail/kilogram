/// Transforms pattern matching expressions into simple matches on primitives.

use super::DesugarPhase;
use crate::typed::typed_node::TypedNode;

pub struct PatternPhase; 

impl DesugarPhase for PatternPhase {
    
    fn transform(&self, root: &TypedNode) -> TypedNode {
        // TODO: reduce CaseOf expression into simplified expression. Return the resultant tree
        todo!()
    }

}
