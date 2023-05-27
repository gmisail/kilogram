use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use crate::{ast::typed::data_type::DataType, ast::untyped::untyped_node::UntypedNode};

fn unify_variant(
    name: &String,
    variants: &BTreeMap<String, Vec<Rc<DataType>>>,
    arguments: &[UntypedNode],
) -> Result<HashMap<String, Rc<DataType>>, String> {
    let variant = variants.get(name);

    match variant {
        Some(types) => {
            if types.is_empty() && arguments.is_empty() {
                // No variables to capture since there are no arguments.
                Ok(HashMap::new())
            } else if !types.is_empty() && types.len() == arguments.len() {
                let mut unbound = HashMap::new();

                // For now, assume every variable keyword is an unbound variable.
                for (index, argument) in arguments.iter().enumerate() {
                    match argument {
                        UntypedNode::Variable(binding_name) => {
                            unbound.insert(binding_name.clone(), types.get(index).unwrap().clone());
                        }

                        // In case we have nested Constructors (i.e. FunctionCall), recursively
                        // unify them.
                        UntypedNode::FunctionCall(parent, _, arguments) => {
                            if let UntypedNode::Variable(name) = &**parent {
                                unbound.extend(unify_variant(name, variants, arguments)?);
                            } else {
                                return Err("Cannot unify non-enum type.".to_string());
                            };
                        }

                        _ => panic!("Unifying with unsupported type."),
                    };
                }

                Ok(unbound)
            } else {
                Err("Variant has incorrect number of types.".to_string())
            }
        }

        None => Err("Cannot find variant.".to_string()),
    }
}

pub fn unify_enum(
    expression: &UntypedNode,
    variants: &BTreeMap<String, Vec<Rc<DataType>>>,
) -> Result<HashMap<String, Rc<DataType>>, String> {
    if let UntypedNode::Variable(name) = expression {
        unify_variant(name, variants, &[])
    } else if let UntypedNode::FunctionCall(parent, _, arguments) = expression {
        if let UntypedNode::Variable(name) = &**parent {
            unify_variant(name, variants, arguments)
        } else {
            Err("Cannot unify non-enum type.".to_string())
        }
    } else {
        Err("Cannot unify non-enum type.".to_string())
    }
}
