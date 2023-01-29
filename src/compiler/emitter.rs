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
        .map(|(field_name, field_type)| format!("{} {}", field_type, field_name))
        .collect();

    format!("typedef struct {{\n\t{}\n}} {};", body.join(",\n\t"), name)
}

pub fn emit_function_call(name: String, is_pointer: bool) -> String {
    if is_pointer {
        format!("(*{})()", name)
    } else {
        format!("{}()", name)
    }
}
