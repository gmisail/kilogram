use crate::ast::untyped::untyped_node::UntypedNode;
use crate::preprocess::generic::data_type_pass::pass::DataTypePass;
use crate::preprocess::generic::function_pass::pass::FunctionPass;

use crate::preprocess::PreprocessPhase;

pub struct GenericPhase {}

impl PreprocessPhase for GenericPhase {
    fn transform(&mut self, root: &UntypedNode) -> UntypedNode {
        // TODO: add function pass, pass to data type pass
        // TODO: add UntypedPass trait for UntypedNode, this will allow us to chain .apply(...) instead.
        let func_pass_res = FunctionPass::new().apply(root);
        let data_pass_res = DataTypePass::new().apply(&func_pass_res);

        println!("{data_pass_res:#?}");

        data_pass_res
    }
}
