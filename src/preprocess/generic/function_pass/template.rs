use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;
use crate::ast::untyped::untyped_node::UntypedNode::*;

use crate::preprocess::generic::template::{substitute_all, Template};

#[derive(Debug)]
pub struct FunctionTemplate {
    type_params: Vec<String>,
    func_params: Vec<(String, AstType)>,
    func_return: AstType,
    func_body: UntypedNode,
}

impl FunctionTemplate {
    pub fn new(
        type_params: Vec<String>,
        func_params: Vec<(String, AstType)>,
        func_return: AstType,
        func_body: UntypedNode,
    ) -> FunctionTemplate {
        FunctionTemplate {
            type_params,
            func_params,
            func_return,
            func_body,
        }
    }

    // From a list of generic parameters and substitutions, get a list of concrete types.
    fn resolve_parameters(&self, substitutions: &[(String, AstType)]) -> Vec<(String, AstType)> {
        self.func_params
            .iter()
            .map(|(param_name, param_type)| {
                let original_type = substitute_all(param_type.clone(), substitutions);

                (param_name.clone(), original_type.as_concrete())
            })
            .collect()
    }

    fn resolve_return_type(&self, substitutions: &[(String, AstType)]) -> AstType {
        substitute_all(self.func_return.clone(), substitutions)
    }

    /*
       TODO: replace all type parameters in the function body with the concrete type
       TODO: re-evaluate the body to check for any unique types
    */
}

impl Template for FunctionTemplate {
    fn substitute(&self, variants: &[AstType], body: UntypedNode) -> UntypedNode {
        match variants {
            [head, tail @ ..] => {
                let types = if let AstType::Generic(_, sub_types) = &head {
                    sub_types
                } else {
                    panic!("Expected type be generic.")
                };

                let substitution_pairs = self
                    .type_params
                    .iter()
                    .cloned()
                    .zip(types.iter().cloned())
                    .collect::<Vec<(String, AstType)>>();

                Let(
                    head.to_string(),
                    None,
                    Box::new(Function(
                        self.resolve_return_type(&substitution_pairs),
                        self.resolve_parameters(&substitution_pairs),
                        Box::new(substitute_body(self.func_body.clone(), &substitution_pairs)),
                    )),
                    Box::new(self.substitute(tail, body)),
                    false,
                )
            }

            [] => body,
        }
    }
}

pub fn substitute_body(root: UntypedNode, substitutions: &Vec<(String, AstType)>) -> UntypedNode {
    match root {
        Integer(..) | Variable(..) | Float(..) | Str(..) | Boolean(..) | Get(..) => root,

        RecordDeclaration(name, fields, type_params, body) => {
            // TODO: check for collisions in types, i.e. if 'T is being substituted, 'T can't be a type param of the record declaration
            RecordDeclaration(
                name.clone(),
                fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        (
                            field_name.clone(),
                            substitute_all(field_type.clone(), substitutions),
                        )
                    })
                    .collect(),
                type_params,
                Box::new(substitute_body(*body, substitutions)),
            )
        }

        Group(body) => substitute_body(*body, substitutions),

        Extern(extern_name, extern_type, body) => Extern(
            extern_name.clone(),
            extern_type.clone(),
            Box::new(substitute_body(*body, substitutions)),
        ),

        Let(name, var_type, var_value, body, is_recursive) => {
            let substituted_type =
                var_type.map(|assigned_type| substitute_all(assigned_type, substitutions));

            Let(
                name.clone(),
                substituted_type,
                Box::new(substitute_body(*var_value, substitutions)),
                Box::new(substitute_body(*body, substitutions)),
                is_recursive,
            )
        }

        Unary(value, operator) => Unary(
            Box::new(substitute_body(*value, substitutions)),
            operator.clone(),
        ),

        Binary(left_value, operator, right_value) => Binary(
            Box::new(substitute_body(*left_value, substitutions)),
            operator.clone(),
            Box::new(substitute_body(*right_value, substitutions)),
        ),

        Logical(left_value, operator, right_value) => Logical(
            Box::new(substitute_body(*left_value, substitutions)),
            operator.clone(),
            Box::new(substitute_body(*right_value, substitutions)),
        ),

        If(if_cond, then_expr, else_expr) => If(
            Box::new(substitute_body(*if_cond, substitutions)),
            Box::new(substitute_body(*then_expr, substitutions)),
            Box::new(substitute_body(*else_expr, substitutions)),
        ),

        Function(return_type, arguments, body) => Function(
            substitute_all(return_type, substitutions),
            arguments
                .iter()
                .map(|(arg_name, arg_type)| {
                    (
                        arg_name.clone(),
                        substitute_all(arg_type.clone(), substitutions),
                    )
                })
                .collect::<Vec<(String, AstType)>>(),
            Box::new(substitute_body(*body, substitutions)),
        ),

        FunctionDeclaration(name, type_params, return_type, arguments, func_body, body) => {
            // TODO: check for collisions with type params

            let substituted_return = substitute_all(return_type, substitutions);
            let substituted_args = arguments
                .iter()
                .map(|(arg_name, arg_type)| {
                    (
                        arg_name.clone(),
                        substitute_all(arg_type.clone(), substitutions),
                    )
                })
                .collect();

            FunctionDeclaration(
                name,
                type_params,
                substituted_return,
                substituted_args,
                Box::new(substitute_body(*func_body, substitutions)),
                Box::new(substitute_body(*body, substitutions)),
            )
        }

        FunctionCall(parent, arguments) => {
            if let FunctionInstance(function, sub_types) = *parent {
                FunctionInstance(
                    Box::new(substitute_body(*function, substitutions)),
                    sub_types
                        .iter()
                        .map(|sub_type| substitute_all(sub_type.clone(), substitutions))
                        .collect(),
                )
            } else {
                FunctionCall(
                    Box::new(substitute_body(*parent, substitutions)),
                    arguments
                        .iter()
                        .map(|argument| substitute_body(argument.clone(), substitutions))
                        .collect(),
                )
            }
        }

        RecordInstance(name, type_params, fields) => RecordInstance(
            name,
            type_params
                .iter()
                .map(|type_param| substitute_all(type_param.clone(), substitutions))
                .collect(),
            fields
                .iter()
                .map(|(field_name, field_value)| {
                    (
                        field_name.clone(),
                        substitute_body(field_value.clone(), substitutions),
                    )
                })
                .collect(),
        ),

        // TODO: check for collisions with type params
        EnumDeclaration(name, variants, type_params, body) => EnumDeclaration(
            name.clone(),
            variants
                .iter()
                .map(|(variant_name, variant_types)| {
                    (
                        variant_name.clone(),
                        variant_types
                            .iter()
                            .map(|variant_type| substitute_all(variant_type.clone(), substitutions))
                            .collect::<Vec<AstType>>(),
                    )
                })
                .collect(),
            type_params.clone(),
            Box::new(substitute_body(*body, substitutions)),
        ),

        List(elements) => List(
            elements
                .iter()
                .map(|element| substitute_body(element.clone(), substitutions))
                .collect(),
        ),

        AnonymousRecord(..) => todo!("handle anonymous records"),
        FunctionInstance(..) => todo!("handle generic functions"),

        CaseOf(_expr, _arms) => {
            todo!("add generic checking to case of")
        }
    }
}
