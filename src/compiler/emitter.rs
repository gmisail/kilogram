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

