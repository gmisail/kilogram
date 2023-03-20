/// Transforms pattern matching expressions into simple matches on primitives.

use super::DesugarPhase;
use crate::typed::typed_node::TypedNode;

pub struct PatternPhase; 

impl DesugarPhase for PatternPhase {
    
    /// Replace the body of each node with its desugared version. All
    /// nodes with no body are unchanged.
    fn transform(&self, root: &TypedNode) -> TypedNode {
        match root {
            TypedNode::Integer(..) | 
                TypedNode::Variable(..) |
                TypedNode::Float(..) | 
                TypedNode::Str(..) | 
                TypedNode::Boolean(..) | 
                TypedNode::Get(..) |
                TypedNode::RecordDeclaration(..) => root.clone(),

            TypedNode::Group(_, body) => {
                self.transform(body)
            },
            
            TypedNode::Let(name, var_type, var_value, body, is_recursive) => {
                TypedNode::Let(name.clone(), var_type.clone(), Box::new(self.transform(&*var_value)), Box::new(self.transform(&*body)), is_recursive.clone())
            }

            _ => todo!()
        }
    }

}

