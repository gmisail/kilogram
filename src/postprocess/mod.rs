/// Postprocessing phases occur after the preprocessing and type-checking phases.
pub mod pattern_phase;

pub trait PostprocessPhase {
    fn transform(&self, root: &TypedNode) -> TypedNode;
}
