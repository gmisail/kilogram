/*
 * Generate common C constructs.
 * */

pub fn emit_unary(operator: String, expression: String) -> String {
    format!("{operator}{expression}")
}

pub fn emit_binary(left: String, operator: String, right: String) -> String {
    format!("{left}{operator}{right}")
}

pub fn emit_if(cond: String, then_expr: String, else_expr: String) -> String {
    format!("if({cond}){{\n{then_expr}\n}} else {{\n{else_expr}\n}}")
}
