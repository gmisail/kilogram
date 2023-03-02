pub enum Pattern {
    Constructor(String, Vec<Pattern>),
    Variable(String)
}
