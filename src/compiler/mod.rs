use std::collections::{BTreeMap, HashMap, HashSet};
use std::env::var;
use std::os::unix::raw::off_t;
use std::rc::Rc;

use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::ast::typed::data_type::DataType;
use crate::ast::typed::typed_node::TypedNode;
use crate::compiler::builder::enum_builder::EnumBuilder;
use crate::compiler::builder::struct_builder::StructBuilder;
use crate::compiler::free::find_free;
use crate::typechecker::Typechecker;

use self::emitter::emit_if;
use self::{
    emitter::{emit_binary, emit_unary},
    generator::FunctionGenerator,
};

mod builder;
mod emitter;
mod free;
mod generator;
mod resolver;

struct FunctionDefinition {
    name: String,
    bound_name: Option<String>,
    data_type: Rc<DataType>,
    arguments: Vec<(String, String)>,
    body: String,
    captures: HashMap<String, Rc<DataType>>,
}

pub struct Compiler {
    // Generates fresh names for functions.
    function: FunctionGenerator,

    // Stores all functions.
    function_header: Vec<FunctionDefinition>,

    // Set of external function names.
    external_function: HashSet<String>,

    // List of type pairs that need helper functions for casting.
    type_casts: HashSet<(String, String)>,

    // Stack of names for referencing previously visited node.
    stack: Vec<String>,

    // Stores all declared records.
    record_types: HashMap<String, Rc<DataType>>,

    // Stores all declared enums.
    enums: HashMap<String, Rc<DataType>>,
}

// TODO: create a table of symbols so we don't need to make new strings every time

impl Compiler {
    pub fn new(typechecker: Typechecker) -> Self {
        Compiler {
            function: FunctionGenerator::new(),
            function_header: Vec::new(),
            external_function: HashSet::new(),
            stack: Vec::new(),
            type_casts: HashSet::new(),
            record_types: typechecker.records,
            enums: typechecker.enums,
        }
    }

    // Wrapper over the resolver module
    fn resolve_type(&self, var_name: &String, var_type: Rc<DataType>) -> String {
        match *var_type {
            DataType::Function(_, _) => format!("KiloFunction* {var_name}"),
            _ => format!("{} {var_name}", resolver::get_native_type(var_type)),
        }
    }

    // Generates a function that constructs a new record from another.
    fn generate_record_cast(&self, from: &String, to: &String) -> String {
        let from_rec = self
            .record_types
            .get(from)
            .expect("Failed to find record with name {from}.");

        let to_rec = self
            .record_types
            .get(from)
            .expect("Failed to find record with name {to}.");

        let mut buffer = String::new();

        buffer.push_str(format!("{to}* {from}_to_{to}({from}* from){{\n").as_str());
        buffer.push_str(format!("{to}* tmp = malloc(sizeof({to}));\n").as_str());

        // Find the shared fields between the two records and generate assignments from the
        // 'from' type into the 'to' type.
        if let DataType::Record(_, from_fields) = &**from_rec {
            let mut from_set = HashSet::new();
            for field_name in from_fields.keys() {
                from_set.insert(field_name);
            }

            if let DataType::Record(_, to_fields) = &**to_rec {
                let mut to_set = HashSet::new();
                for field_name in to_fields.keys() {
                    to_set.insert(field_name);
                }

                let shared_fields = from_set.intersection(&to_set);

                for field_name in shared_fields {
                    buffer.push_str(format!("tmp->{field_name} = to->{field_name};\n").as_str());
                }
            }
        }

        buffer.push_str("return tmp;\n");
        buffer.push_str("}\n");

        buffer
    }

    /// Generates all enum declarations & constructors.
    fn generate_enum_header(&self) -> String {
        let mut buffer = String::new();

        // Declare forward declarations.
        for enum_name in self.enums.keys() {
            buffer.push_str(format!("typedef struct {enum_name} {enum_name};\n").as_str());
        }

        for (enum_name, enum_def) in &self.enums {
            let mut builder = EnumBuilder::new(enum_name.clone());

            if let DataType::Enum(_, enum_variants) = &**enum_def {
                for (variant_name, variant_types) in enum_variants {
                    let mut resolved_variant_types = Vec::new();

                    for variant_type in variant_types {
                        resolved_variant_types
                            .push(resolver::get_native_type(variant_type.clone()));
                    }

                    builder.variant(variant_name.clone(), resolved_variant_types);
                }
            }

            buffer.push_str(&builder.build());
            buffer.push_str(&builder.build_constructors());
        }

        buffer
    }

    // Generates the header of forward declarations and the struct definitions.
    fn generate_record_header(&self) -> String {
        let mut buffer = String::new();

        // Declare forward declarations.
        for record_name in self.record_types.keys() {
            buffer.push_str(format!("typedef struct {record_name} {record_name};\n").as_str());
        }

        // Generate a named struct and its constructor.
        for (name, record_type) in &self.record_types {
            let record_fields = match &**record_type {
                DataType::Record(_, fields) => fields,

                _ => panic!(),
            };

            // Define the record as a C struct.
            let mut record_struct = StructBuilder::new(name.clone());

            for (field_name, field_type) in record_fields {
                record_struct.field(
                    field_name.clone(),
                    resolver::get_native_type(field_type.clone()),
                );
            }

            buffer.push_str(&record_struct.build(false));
            buffer.push_str(&record_struct.build_constructor());
        }

        // Generate helper functions for casting between record types.
        for (expected_name, actual_name) in &self.type_casts {
            buffer.push_str(&self.generate_record_cast(actual_name, expected_name));
        }

        buffer
    }

    /// Generates a function which will allocate a function, load the environment,
    /// and return its reference.
    ///
    /// * `name`: name of the function
    /// * `bound_name`: optionally the name that it's assigned to
    /// * `free_vars`: variables captured by the function
    fn generate_function_constructor(
        &self,
        name: &String,
        bound_name: &Option<String>,
        free_vars: &HashMap<String, Rc<DataType>>,
    ) -> String {
        let mut buffer = String::new();

        buffer.push_str(format!("KiloFunction* create_{name}").as_str());

        buffer.push('(');
        buffer.push_str(
            free_vars
                .iter()
                .filter(|(var_name, _)| match bound_name {
                    Some(name) => **var_name != name.clone(),
                    None => true,
                })
                .map(|(var_name, var_type)| {
                    format!("{} {var_name}", resolver::get_native_type(var_type.clone()))
                })
                .collect::<Vec<String>>()
                .join(", ")
                .as_str(),
        );
        buffer.push(')');
        buffer.push_str("{\n");

        let tmp_name = bound_name.clone().unwrap_or_else(|| "tmp".to_string());

        buffer.push_str(format!("KiloFunction* {tmp_name} = function_create({name});\n").as_str());
        buffer.push_str(
            format!(
                "{tmp_name}->env = _create_{name}_env({});\n",
                free_vars.keys().cloned().collect::<Vec<String>>().join(",")
            )
            .as_str(),
        );

        buffer.push_str(format!("return {tmp_name};\n}}\n").as_str());

        buffer
    }

    // Generates the header of forward declarations & function definitions.
    fn generate_function_header(&mut self) -> String {
        let mut buffer = String::new();

        // TODO: have forward declarations before defining functions

        for def in &self.function_header {
            let mut args = def
                .arguments
                .iter()
                .map(|(_, arg_type)| arg_type.clone())
                .collect::<Vec<String>>();

            args.push(format!("{}_env* env", def.name));
            let arg_buffer = args.join(", ");

            let mut func_env = StructBuilder::new(format!("{}_env", def.name));

            for (field_name, field_type) in &def.captures {
                func_env.field(
                    field_name.clone(),
                    resolver::get_native_type(field_type.clone()),
                );
            }

            // Brings variables from environment back into scope.
            let hydrate_env = def
                .captures
                .iter()
                .map(|(env_name, env_type)| {
                    format!(
                        "{} {env_name} = env->{env_name};",
                        resolver::get_native_type(env_type.clone())
                    )
                })
                .collect::<Vec<String>>()
                .join("\n");

            let func_buffer = format!(
                "{} ({}){{\n{}\n{}\n}}",
                self.resolve_type(&def.name, def.data_type.clone()),
                arg_buffer,
                hydrate_env,
                def.body
            );

            buffer.push_str(&func_env.build(true));
            buffer.push('\n');
            buffer.push_str(&func_env.build_constructor());
            buffer.push('\n');
            buffer.push_str(&func_buffer);
            buffer.push('\n');
            buffer.push_str(&self.generate_function_constructor(
                &def.name,
                &def.bound_name,
                &def.captures,
            ));
            buffer.push('\n');
        }

        buffer
    }

    pub fn compile(&mut self, expression: &TypedNode) -> String {
        let mut buffer = String::new();

        let root_expr = self.compile_expression(expression);

        buffer.push_str("#include<stdio.h>\n");
        buffer.push_str("#include<stdlib.h>\n");

        buffer.push_str("#include \"runtime/string.h\"\n");
        buffer.push_str("#include \"runtime/object.h\"\n");
        buffer.push_str("#include \"runtime/function.h\"\n");
        buffer.push_str("#include \"runtime/stdlib.h\"\n");

        buffer.push_str("\n// Record header\n");
        buffer.push_str(&self.generate_record_header());
        buffer.push_str("\n// Enum header\n");
        buffer.push_str(&self.generate_enum_header());
        buffer.push_str("\n// Function header\n");
        buffer.push_str(&self.generate_function_header());
        buffer.push_str("\n// Program\n");

        // Inject the source into the main function
        buffer.push_str("int main(int argc, char** argv){\n");

        // Inject any Kilogram-specific C code here.
        buffer.push_str(&root_expr);

        // Destructors
        buffer.push_str("\n}");

        buffer
    }

    fn compile_unary(&mut self, expression: &TypedNode, operation: &UnaryOperator) -> String {
        let unary_symbol = match operation {
            UnaryOperator::Minus => "-".to_string(),
            UnaryOperator::Bang => "!".to_string(),
        };

        emit_unary(unary_symbol, self.compile_expression(expression))
    }

    fn compile_binary(
        &mut self,
        left: &TypedNode,
        right: &TypedNode,
        operation: &BinaryOperator,
    ) -> String {
        let binary_symbol = match operation {
            BinaryOperator::Add => "+".to_string(),
            BinaryOperator::Sub => "-".to_string(),
            BinaryOperator::Mult => "*".to_string(),
            BinaryOperator::Div => "/".to_string(),
            BinaryOperator::Equality => "==".to_string(),
            BinaryOperator::NotEqual => "!=".to_string(),
            BinaryOperator::Greater => ">".to_string(),
            BinaryOperator::GreaterEq => ">=".to_string(),
            BinaryOperator::Less => "<".to_string(),
            BinaryOperator::LessEq => "<=".to_string(),
        };

        emit_binary(
            self.compile_expression(left),
            binary_symbol,
            self.compile_expression(right),
        )
    }

    fn compile_logical(
        &mut self,
        left: &TypedNode,
        right: &TypedNode,
        operation: &LogicalOperator,
    ) -> String {
        let logical_symbol = match operation {
            LogicalOperator::And => "&&",
            LogicalOperator::Or => "||",
        };

        emit_binary(
            self.compile_expression(left),
            logical_symbol.to_string(),
            self.compile_expression(right),
        )
    }

    fn compile_let(
        &mut self,
        name: &String,
        var_type: &Rc<DataType>,
        value: &TypedNode,
        body: &TypedNode,
        is_recursive: bool,
    ) -> String {
        // Is the last node not a declaration? Return its value.
        let is_leaf = !matches!(*body, TypedNode::Let(..));

        if is_recursive {
            self.stack.push(name.clone());
        }

        format!(
            "{} = {};\n{}{}{}",
            self.resolve_type(name, var_type.clone()),
            self.compile_expression(value),
            if is_leaf { "return " } else { "" },
            self.compile_expression(body),
            if is_leaf { ";" } else { "" }
        )
    }

    /// Define the lambda under a unique name and returns a pointer
    /// to it.
    ///
    /// * `func_type`:
    /// * `arg_types`:
    /// * `value`:
    fn compile_function(
        &mut self,
        func_type: &Rc<DataType>,
        arg_types: &[(String, Rc<DataType>)],
        value: &TypedNode,
        free_vars: HashMap<String, Rc<DataType>>,
    ) -> String {
        // Generate fresh name.
        let fresh_name = self.function.generate();
        let is_leaf = !matches!(*value, TypedNode::Let(..));

        let func_body = format!(
            "{}{}{}",
            if is_leaf { "return " } else { "" },
            self.compile_expression(value),
            if is_leaf { ";" } else { "" }
        );

        let arguments = arg_types
            .iter()
            .map(|(arg_name, arg_type)| {
                (
                    arg_name.clone(),
                    self.resolve_type(arg_name, arg_type.clone()),
                )
            })
            .collect();

        let bound_by = self.stack.pop();

        self.function_header.push(FunctionDefinition {
            name: fresh_name.clone(),
            bound_name: bound_by.clone(),
            data_type: func_type.clone(),
            arguments,
            body: func_body,
            captures: free_vars.clone(),
        });

        // If the function is recursive, filter out the reference to itself (since it's impossible
        // to pass a reference to something that doesn't exist yet.)
        let free_args: Vec<String> = free_vars
            .keys()
            .cloned()
            .filter(|name| {
                if let Some(rec_name) = &bound_by {
                    name != rec_name
                } else {
                    true
                }
            })
            .collect();

        // All user-declared functions are a pointer to a function in the function header.
        format!("create_{fresh_name}({})", free_args.join(", "))
    }

    /// Generates a function call given a function name and arguments.
    ///
    /// * `name`:
    /// * `arguments`:
    fn compile_function_call(&mut self, name: &TypedNode, arguments: &[TypedNode]) -> String {
        let argument_list: Vec<String> = arguments
            .iter()
            .map(|arg| self.compile_expression(arg))
            .collect();

        let mut is_extern = false;

        let base_type = match name {
            TypedNode::Variable(func_type, func_name) => {
                is_extern = self.external_function.contains(func_name);

                func_type
            }
            _ => panic!("Do not support calling non-variables yet"),
        };

        if !is_extern {
            let func_type = resolver::get_function_pointer("".to_string(), base_type.clone());
            let function = self.compile_expression(name);

            format!(
                "(({func_type}) {function}->body)({}, {function}->env)",
                argument_list.join(", "),
            )
        } else {
            let base_expr = self.compile_expression(name);
            format!("{base_expr}({})", argument_list.join(", "))
        }
    }

    fn compile_record_instance(&mut self, name: &String, fields: &[(String, TypedNode)]) -> String {
        let mut buffer = String::new();

        // To make sure that the fields are inserted in the same order that they're stored.
        let mut ordered_fields: BTreeMap<String, &TypedNode> = BTreeMap::new();

        for (field_name, field_value) in fields {
            ordered_fields.insert(field_name.clone(), field_value);
        }

        buffer.push_str(format!("_create_{name}(").as_str());

        let field_expressions = ordered_fields
            .values()
            .map(|field_value| self.compile_expression(field_value))
            .collect::<Vec<String>>();

        buffer.push_str(field_expressions.join(", ").as_str());
        buffer.push(')');

        buffer
    }

    /// Generates a condition for checking if an expression is of a variant type.
    fn get_expression_variant_check(
        &mut self,
        expression: &TypedNode,
        enum_type: Rc<DataType>,
        constructor: &String,
    ) -> String {
        let compiled_target = self.compile_expression(expression);

        if let DataType::Enum(_, variants) = &*enum_type {
            let (index, variant) = variants
                .keys()
                .enumerate()
                .find(|(_, variant_name)| *variant_name == constructor)
                .expect("Cannot find variant in enum.");

            format!("{compiled_target}->id == {index}")
        } else {
            panic!("Cannot create expression-variant check for non-enum type.")
        }
    }

    fn generate_let_chain(&self, variables: &[TypedNode], default: &TypedNode) -> TypedNode {
        match variables {
            [head, tail @ ..] => {
                if let TypedNode::Variable(var_type, var_name) = head {
                    TypedNode::Let(
                        var_name.clone(),
                        var_type.clone(),
                        Box::new(head.clone()),
                        Box::new(self.generate_let_chain(tail, default)),
                        false,
                    )
                } else {
                    panic!()
                }
            }
            [] => default.clone(),
        }
    }

    fn compile_case_of(
        &mut self,
        expression: &TypedNode,
        arms: &[(TypedNode, TypedNode, HashMap<String, Rc<DataType>>)],
    ) -> String {
        let match_on = self.compile_expression(expression);
        let mut buffer = String::new();

        for (arm_cond, arm_body, _) in arms {
            match arm_cond {
                TypedNode::Variable(var_type, var_name) => {
                    buffer.push_str("else {\n");

                    buffer.push_str(
                        self.compile_expression(&TypedNode::Let(
                            var_name.to_string(),
                            var_type.clone(),
                            Box::new(expression.clone()),
                            Box::new(arm_body.clone()),
                            false,
                        ))
                        .as_str(),
                    );

                    buffer.push_str("}\n");
                }

                TypedNode::EnumInstance(enum_type, constructor_name, bindings) => {
                    buffer.push_str(
                        format!(
                            "// {constructor_name}\nif({}){{\n",
                            self.get_expression_variant_check(
                                expression,
                                enum_type.clone(),
                                constructor_name,
                            )
                        )
                        .as_str(),
                    );

                    buffer.push_str(format!("Bindings: {:?}\n", bindings).as_str());

                    buffer.push_str(
                        &self.compile_expression(&self.generate_let_chain(bindings, arm_body)),
                    );

                    buffer.push_str("}");
                }
                _ => panic!(),
            }
        }

        buffer.push_str("}");

        buffer
    }

    fn compile_enum_instance(
        &mut self,
        enum_type: &Rc<DataType>,
        variant_name: &String,
        variant_arguments: &[TypedNode],
    ) -> String {
        if let DataType::Enum(enum_name, _) = &**enum_type {
            let mut arguments = Vec::new();

            for argument in variant_arguments {
                arguments.push(self.compile_expression(argument));
            }

            format!(
                "_create_{enum_name}_{variant_name}({})",
                arguments.join(", ")
            )
        } else {
            panic!("Type in Enum is not actually an Enum!")
        }
    }

    /// Compiles an expression.
    ///
    /// * `expression`: expression to compile into a String.
    pub fn compile_expression(&mut self, expression: &TypedNode) -> String {
        match expression {
            TypedNode::Integer(_, value) => format!("{value}"),
            TypedNode::Float(_, value) => format!("{value}"),
            TypedNode::Str(_, value) => format!("string_create(\"{value}\")"),
            TypedNode::Boolean(_, value) => format!("{value}"),
            TypedNode::Variable(_, name) => name.clone(),
            TypedNode::Group(_, expr) => format!("({})", self.compile_expression(expr)),

            TypedNode::Unary(_, expr, operation) => self.compile_unary(expr, operation),
            TypedNode::Binary(_, left, operation, right) => {
                self.compile_binary(left, right, operation)
            }
            TypedNode::Logical(_, left, operation, right) => {
                self.compile_logical(left, right, operation)
            }

            TypedNode::If(_, if_expr, then_expr, else_expr) => emit_if(
                self.compile_expression(if_expr),
                self.compile_expression(then_expr),
                self.compile_expression(else_expr),
            ),

            TypedNode::CaseOf(_, expr, arms) => self.compile_case_of(expr, arms),

            TypedNode::Let(name, var_type, value, body, is_rec) => {
                self.compile_let(name, var_type, value, body, *is_rec)
            }

            TypedNode::Function(_, func_type, arg_types, value) => {
                let mut free_vars = find_free(expression);

                // External functions can be accessed without being passed down as
                // and environment variable.
                for extern_type in &self.external_function {
                    free_vars.remove(extern_type);
                }

                self.compile_function(func_type, arg_types, value, free_vars)
            }

            TypedNode::Get(_, name, parent) => {
                format!("{}->{name}", self.compile_expression(parent))
            }

            TypedNode::FunctionCall(_, name, arguments) => {
                self.compile_function_call(name, arguments)
            }

            // Just ignore record declarations, already handled in the record header.
            TypedNode::RecordDeclaration(_, _, body) => self.compile_expression(body),
            TypedNode::RecordInstance(name, fields) => self.compile_record_instance(name, fields),

            // Just ignore extern definitions, don't actually compile to anything.
            TypedNode::Extern(name, extern_type, body) => {
                // If a function, add to the list of functions.
                if matches!(**extern_type, DataType::Function(..)) {
                    self.external_function.insert(name.clone());
                }

                self.compile_expression(body)
            }

            TypedNode::EnumInstance(enum_type, variant, variant_arguments) => {
                self.compile_enum_instance(enum_type, variant, variant_arguments)
            }
        }
    }
}
