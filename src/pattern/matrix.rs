struct Matrix {
    patterns: Vec<Vec<Pattern>>,
    expressions: Vec<i32>,
    variables: Vec<String>
}

impl Matrix {
    pub fn new() -> Self {
        
    }

    pub fn compile(&mut self) -> Result<(), String> {
        if self.patterns.is_empty() {
            // No patterns? In-exhaustive branch.
            
            Err(String::from("Failure"))
        } else if self.patterns.first().unwrap().iter().all(|pattern| pattern == &Pattern::Wildcard || pattern == &Pattern::Variable(..)) {
            // If the first pattern is all wildcards & variables, it will match.
            
            Ok(self.expressions.first().unwrap().clone())
        } else {
            todo!()
        }
    }
}
