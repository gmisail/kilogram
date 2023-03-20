/// Handles desugaring expressions into an expanded form.
use crate::typed::typed_node::TypedNode;

pub mod pattern;

/// Represents a phase during the desugaring process. Each phase performs a
/// single operation.
pub trait DesugarPhase {
    fn transform(&self, root: &TypedNode) -> TypedNode;
}
