#[derive(Clone)]
pub enum Type {
    Integer,
    Float,
    Str,
    Boolean,

    // Make this a pointer instead? i.e. Box<&Type>
    Function(Vec<Box<Type>>, Box<Type>),
    Record(String, Vec<(String, Box<Type>)>),
}
