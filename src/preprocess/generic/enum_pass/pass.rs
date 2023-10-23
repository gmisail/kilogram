use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;
use crate::preprocess::generic::enum_pass::template::EnumTemplate;
use crate::preprocess::generic::pass_state::PassState;
use crate::preprocess::generic::phase::ConcretePass;

pub struct EnumPass {
    state: PassState<EnumTemplate>,
}

impl EnumPass {
    pub fn new() -> Self {
        EnumPass {
            state: PassState::new(),
        }
    }
}

impl ConcretePass for EnumPass {
    fn apply(&mut self, node: &UntypedNode) -> UntypedNode {
        todo!()
    }

    fn register_type(&mut self, name: String, ast_type: AstType) {
        todo!()
    }

    fn find_unique_types(&mut self, root: &UntypedNode) {
        todo!()
    }

    fn expand_generic_declarations(&mut self, root: &UntypedNode) -> UntypedNode {
        todo!()
    }
}
