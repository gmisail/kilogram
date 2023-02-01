/*
 * Generate common C constructs.
 * */

pub fn emit_unary(operator: String, expression: String) -> String {
    format!("{}{}", operator, expression)
}

pub fn emit_binary(left: String, operator: String, right: String) -> String {
    format!("{}{}{}", left, operator, right)
}

pub fn emit_logical(left: String, operator: String, right: String) -> String {
    emit_binary(left, operator, right)
}

pub fn emit_struct(name: String, fields: Vec<(String, String)>) -> String {
    let body: Vec<String> = fields
        .iter()
        .map(|(field_name, field_type)| format!("{} {};", field_type, field_name))
        .collect();

    format!("typedef struct {{\n\t{}\n}} {};", body.join("\n\t"), name)
}

pub fn emit_function_call(name: String, arguments: &[String], is_pointer: bool) -> String {
    if is_pointer {
        format!("(*{})({})", name, arguments.join(", "))
    } else {
        format!("{}({})", name, arguments.join(", "))
    }
}

pub fn emit_if(cond: String, then_expr: String, else_expr: String) -> String {
    format!("{} ? {} : {}", cond, then_expr, else_expr)
}
