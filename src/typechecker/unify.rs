use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use crate::{ast::untyped::untyped_node::UntypedNode, ast::typed::data_type::DataType};

fn unify_variant(
    name: &String,
    variants: &BTreeMap<String, Vec<Rc<DataType>>>,
    arguments: &[UntypedNode],
) -> Result<HashMap<String, Rc<DataType>>, String> {
    let variant = variants.get(name);

    match variant {
        Some(types) => {
            if types.len() == 0 && arguments.len() == 0 {
                // No variables to capture since there are no arguments.
                Ok(HashMap::new())
            } else if types.len() > 0 && types.len() == arguments.len() {
                let mut unbound = HashMap::new();

                // TODO: make this more robust, more like a pattern matching system.

                // For now, assume every variable keyword is an unbound variable.
                for (index, argument) in arguments.iter().enumerate() {
                    if let UntypedNode::Variable(binding_name) = argument {
                        unbound.insert(binding_name.clone(), types.get(index).unwrap().clone());
                    }
                }

                Ok(unbound)
            } else {
                Err("Variant has incorrect number of types.".to_string())
            }
        }

        None => return Err("Cannot find variant.".to_string()),
    }
}

pub fn unify_enum(
    expression: &UntypedNode,
    variants: &BTreeMap<String, Vec<Rc<DataType>>>,
) -> Result<HashMap<String, Rc<DataType>>, String> {
    if let UntypedNode::Variable(name) = expression {
        unify_variant(name, variants, &[])
    } else if let UntypedNode::FunctionCall(parent, arguments) = expression {
        if let UntypedNode::Variable(name) = &**parent {
            unify_variant(name, variants, &arguments)
        } else {
            Err("Cannot unify non-enum type.".to_string())
        }
    } else {
        Err("Cannot unify non-enum type.".to_string())
    }
}
