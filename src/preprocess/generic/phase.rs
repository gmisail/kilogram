use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;
use crate::preprocess::generic::data_type_pass::pass::DataTypePass;
use crate::preprocess::generic::enum_pass::pass::EnumPass;
use crate::preprocess::generic::function_pass::pass::FunctionPass;

use crate::preprocess::PreprocessPhase;

pub trait ConcretePass {
    fn apply(&mut self, node: &UntypedNode) -> UntypedNode {
        // First, search the AST for usages of generic types.
        self.find_unique_types(node);

        // Once these are found, convert them into concrete type declarations.
        self.expand_generic_declarations(node)
    }

    fn register_type(&mut self, name: String, ast_type: AstType);
    fn find_unique_types(&mut self, root: &UntypedNode);
    fn expand_generic_declarations(&mut self, root: &UntypedNode) -> UntypedNode;

    /// Recurse through a type searching for unique generic type instances, creating them
    /// if they do not exist.
    ///
    /// * `ast_type`: type to search on
    fn resolve_generic_type(&mut self, ast_type: &AstType) {
        match ast_type {
            // Base types (int, float, etc...) can't be generic.
            AstType::Base(_) => (),

            AstType::Generic(name, sub_types) => {
                for sub_type in sub_types {
                    self.resolve_generic_type(sub_type);
                }

                let new_type = AstType::Generic(name.clone(), sub_types.clone());

                self.register_type(name.clone(), new_type);
            }

            AstType::Function(arguments, return_type) => {
                for argument in arguments {
                    self.resolve_generic_type(argument);
                }

                self.resolve_generic_type(return_type);
            }
            AstType::Record(fields) => {
                for (_, field_type) in fields {
                    self.resolve_generic_type(field_type);
                }
            }
        }
    }
}

pub struct GenericPhase;

impl PreprocessPhase for GenericPhase {
    fn transform(&mut self, root: &UntypedNode) -> UntypedNode {
        // TODO: add function pass, pass to data type pass
        // TODO: add UntypedPass trait for UntypedNode, this will allow us to chain .apply(...) instead.

        let func_pass_res = FunctionPass::new().apply(root);
        let data_pass_res = DataTypePass::new().apply(&func_pass_res);
        let enum_pass_res = EnumPass::new().apply(&data_pass_res);

        enum_pass_res
    }
}
