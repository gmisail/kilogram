use crate::ast::untyped::untyped_node::UntypedNode;

use self::{data_type_pass::DataTypePass, function_pass::FunctionPass};

mod data_type_pass;
mod function_pass;

pub fn apply_passes(root: &UntypedNode) -> UntypedNode {
    // TODO: add function pass, pass to data type pass
    let func_pass_res = FunctionPass::new().apply(root);

    DataTypePass::new().apply(&func_pass_res)
}
