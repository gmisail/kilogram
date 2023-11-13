use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;
use crate::ast::untyped::untyped_node::UntypedNode::*;
use crate::preprocess::generic::function_pass::template::FunctionTemplate;
use crate::preprocess::generic::pass_state::PassState;
use crate::preprocess::generic::phase::ConcretePass;
use std::collections::HashSet;

use crate::preprocess::generic::template::Template;

///
///     GENERIC - FUNCTION PASS
///
///     Walks through the AST looking for
///         a) generic type declarations
///         b) unique invocations of generic types
///
///     Given these unique invocations, walk through again and replace the generic calls
///     with calls to concrete types.
///

// TODO: add type parameter collision detection, i.e. if there is 'T defined within a generic
// type that defines 'T, throw an error.

pub struct FunctionPass {
    state: PassState<FunctionTemplate>,
    enums: HashSet<String>,
}

impl FunctionPass {
    pub fn new() -> Self {
        FunctionPass {
            state: PassState::new(),
            enums: HashSet::new(),
        }
    }
}

impl ConcretePass for FunctionPass {
    fn register_type(&mut self, name: String, ast_type: AstType) {
        self.state.register_type(name, ast_type);
    }

    /// Search the AST for generic types and save all unique configurations.
    ///  i.e. Pair(int, int), Pair(float, int), etc...
    ///
    /// * `root`: node to search
    fn find_unique_types(&mut self, root: &UntypedNode) {
        match root {
            Function(return_type, arguments, body) => {
                // Find generic types in both the return type and arguments.
                self.resolve_generic_type(return_type);

                for (_, arg_type) in arguments {
                    self.resolve_generic_type(arg_type);
                }

                self.find_unique_types(body);
            }

            FunctionCall(parent, arguments) => {
                self.find_unique_types(parent);

                if let FunctionInstance(base, sub_types) = &**parent {
                    for sub_type in sub_types {
                        self.resolve_generic_type(sub_type);
                    }

                    if let Variable(function_name) = &**base {
                        // TODO: maybe we should separate this into separate sets, i.e. not mix functions and other types?
                        self.state.register_type(
                            function_name.clone(),
                            AstType::Generic(function_name.clone(), sub_types.clone()),
                        );
                    } else {
                        panic!("Generic functions must be named.")
                    }
                }

                for argument in arguments {
                    self.find_unique_types(argument);
                }
            }

            FunctionDeclaration(name, type_params, return_type, func_params, func_body, body) => {
                // Find generic types in both the return type and arguments.
                self.resolve_generic_type(return_type);

                for (_, param_type) in func_params {
                    self.resolve_generic_type(param_type);
                }

                if !type_params.is_empty() {
                    let inserted_template = self.state.register_template(
                        name.clone(),
                        FunctionTemplate::new(
                            type_params.clone(),
                            func_params.clone(),
                            return_type.clone(),
                            (**func_body).clone(),
                        ),
                    );

                    if inserted_template.is_some() {
                        panic!(
                            "TODO: handle this gracefully. This function template already exists."
                        );
                    }
                }

                self.find_unique_types(func_body);
                self.find_unique_types(body);
            }

            FunctionInstance(parent, sub_types) => {
                let name = match &**parent {
                    Variable(function_name) => function_name,
                    _ => panic!("expected generic function call to be named."),
                };

                for sub_type in sub_types {
                    self.resolve_generic_type(sub_type);
                }

                self.state.register_type(
                    name.clone(),
                    AstType::Generic(name.clone(), sub_types.clone()),
                );
            }

            Integer(..) | Variable(..) | Float(..) | Str(..) | Boolean(..) => {}

            Get(_, parent) => {
                self.find_unique_types(parent);
            }

            Group(body) | Extern(_, _, body) => {
                self.find_unique_types(body);
            }

            Let(_, var_type, var_value, body, _) => {
                // If type is explicit, check if it contains a generic type.
                if let Some(t) = var_type {
                    self.resolve_generic_type(t);
                }

                self.find_unique_types(var_value);
                self.find_unique_types(body);
            }

            Unary(value, _) => {
                self.find_unique_types(value);
            }

            Binary(left_value, _, right_value) | Logical(left_value, _, right_value) => {
                self.find_unique_types(left_value);
                self.find_unique_types(right_value);
            }

            If(if_cond, then_expr, else_expr) => {
                self.find_unique_types(if_cond);
                self.find_unique_types(then_expr);
                self.find_unique_types(else_expr);
            }

            RecordInstance(_, _, fields) => {
                for (_, field_value) in fields {
                    self.find_unique_types(field_value);
                }
            }

            RecordDeclaration(_, _, _, body) => self.find_unique_types(body),

            EnumDeclaration(_, variants, _, body) => {
                for (_, variant_types) in variants {
                    for variant_type in variant_types {
                        self.resolve_generic_type(variant_type);
                    }
                }

                self.find_unique_types(body);
            }

            List(elements) => {
                for element in elements {
                    self.find_unique_types(element);
                }
            }

            AnonymousRecord(..) => todo!("add generic checking to anonymous records"),
            CaseOf(_expr, _arms) => todo!("add generic checking to case of"),
        }
    }

    /// Given a root, expand the generic declarations into monomorphized versions.
    ///     i.e. Pair('T, 'S) with (int, int) & (float, string) => Pair_int_int, Pair_float_string
    ///
    /// * `root`: node to start expansion
    fn expand_generic_declarations(&mut self, root: &UntypedNode) -> UntypedNode {
        match root {
            Integer(..) | Variable(..) | Float(..) | Str(..) | Boolean(..) => root.to_owned(),

            Get(_, parent) => self.expand_generic_declarations(parent),

            RecordDeclaration(name, fields, type_params, body) => RecordDeclaration(
                name.clone(),
                fields.clone(),
                type_params.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            Group(body) => self.expand_generic_declarations(body),

            Extern(extern_name, extern_type, body) => Extern(
                extern_name.clone(),
                extern_type.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            Let(name, var_type, var_value, body, is_recursive) => Let(
                name.clone(),
                var_type.clone(),
                Box::new(self.expand_generic_declarations(var_value)),
                Box::new(self.expand_generic_declarations(body)),
                *is_recursive,
            ),

            Unary(value, operator) => Unary(
                Box::new(self.expand_generic_declarations(value)),
                operator.clone(),
            ),

            Binary(left_value, operator, right_value) => Binary(
                Box::new(self.expand_generic_declarations(left_value)),
                operator.clone(),
                Box::new(self.expand_generic_declarations(right_value)),
            ),

            Logical(left_value, operator, right_value) => Logical(
                Box::new(self.expand_generic_declarations(left_value)),
                operator.clone(),
                Box::new(self.expand_generic_declarations(right_value)),
            ),

            If(if_cond, then_expr, else_expr) => If(
                Box::new(self.expand_generic_declarations(if_cond)),
                Box::new(self.expand_generic_declarations(then_expr)),
                Box::new(self.expand_generic_declarations(else_expr)),
            ),

            Function(return_type, arguments, body) => Function(
                return_type.clone(),
                arguments.clone(),
                Box::new(self.expand_generic_declarations(body)),
            ),

            FunctionDeclaration(name, type_params, return_type, arguments, func_body, body) => {
                if !type_params.is_empty() {
                    // Expand the rest of the code first.
                    let expanded_body = self.expand_generic_declarations(body);

                    // Find the respective template, unique type parameters.
                    let template = self.state.get_template(name).unwrap();

                    // In case there are no instances of this generic functions, ignore it.
                    if let Some(types) = self.state.get_types(name) {
                        // Given these types, generate copies of the function template.
                        template.substitute(types, expanded_body)
                    } else {
                        expanded_body
                    }
                } else {
                    /*
                        Not generic? Don't apply any substitutions, just convert types to concrete
                        and recurse.
                    */
                    // FunctionDeclaration(
                    //     name.clone(),
                    //     Vec::new(),
                    //     return_type.as_concrete(),
                    //     arguments
                    //         .iter()
                    //         .map(|(arg_name, arg_type)| {
                    //             (arg_name.clone(), arg_type.as_concrete())
                    //         })
                    //         .collect(),
                    //     Box::new(self.expand_generic_declarations(func_body)),
                    //     Box::new(self.expand_generic_declarations(body)),
                    // )

                    FunctionDeclaration(
                        name.clone(),
                        Vec::new(),
                        return_type.clone(),
                        arguments.clone(),
                        Box::new(self.expand_generic_declarations(func_body)),
                        Box::new(self.expand_generic_declarations(body)),
                    )
                }
            }

            FunctionCall(parent, arguments) => {
                // If generic, we can assume that the function's name must be a named function.
                // Otherwise, keep expanding as normal
                let new_name = if let FunctionInstance(function, sub_types) = &**parent {
                    let original_name = match &**function {
                        Variable(original_name) => original_name,
                        _ => panic!("expected generic function call to be named."),
                    };

                    let concrete_name = AstType::Generic(
                        original_name.clone(),
                        sub_types
                            .iter()
                            .map(|sub_type| sub_type.as_concrete())
                            .collect(),
                    )
                    .to_string();

                    Variable(concrete_name)
                } else {
                    self.expand_generic_declarations(parent)
                };

                FunctionCall(
                    Box::new(new_name),
                    arguments
                        .iter()
                        .map(|argument| self.expand_generic_declarations(argument))
                        .collect(),
                )
            }

            RecordInstance(name, type_params, fields) => RecordInstance(
                name.clone(),
                type_params.clone(),
                fields
                    .iter()
                    .map(|(field_name, field_value)| {
                        (
                            field_name.clone(),
                            self.expand_generic_declarations(field_value),
                        )
                    })
                    .collect(),
            ),

            EnumDeclaration(name, variants, type_params, body) => {
                self.enums.insert(name.clone());

                EnumDeclaration(
                    name.clone(),
                    variants.clone(),
                    // .iter()
                    // .map(|(variant_name, variant_types)| {
                    //     (
                    //         variant_name.clone(),
                    //         variant_types
                    //             .iter()
                    //             .map(|variant_type| variant_type.as_concrete())
                    //             .collect(),
                    //     )
                    // })
                    // .collect(),
                    type_params.clone(),
                    Box::new(self.expand_generic_declarations(body)),
                )
            }

            List(elements) => List(
                elements
                    .iter()
                    .map(|element| self.expand_generic_declarations(element))
                    .collect(),
            ),

            FunctionInstance(base, sub_types) => {
                let original_name = match &**base {
                    Variable(original_name) => original_name,
                    _ => panic!("expected generic function call to be named."),
                };

                // let resolved_sub_types = sub_types
                //     .iter()
                //     .map(|sub_type| sub_type.as_concrete())
                //     .collect();

                // Function instances are tricky -- at this phase, they
                // also represent generic enums since they're identical at a syntax level. So, we
                // need to check what we're dealing with to make sure we're not converting an enum
                // into a concrete type (we do that later.)
                if self.enums.contains(original_name) {
                    FunctionInstance(base.clone(), sub_types.clone())
                } else {
                    let concrete_name =
                        AstType::Generic(original_name.clone(), sub_types.clone()).to_string();

                    Variable(concrete_name)
                }
            }

            AnonymousRecord(..) => todo!("handle anonymous records"),

            CaseOf(_expr, _arms) => {
                todo!("add generic checking to case of")
            }
        }
    }
}
