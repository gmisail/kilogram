pub struct EnumBuilder {
    name: String,
    variants: Vec<(String, Vec<String>)>,
}

impl EnumBuilder {
    pub fn new(name: String) -> Self {
        EnumBuilder {
            name,
            variants: Vec::new(),
        }
    }

    pub fn variant(
        &mut self,
        variant_name: String,
        variant_types: Vec<String>,
    ) -> &mut EnumBuilder {
        self.variants.push((variant_name, variant_types));

        self
    }

    pub fn build(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(format!("struct {} {{\n", self.name).as_str());
        buffer.push_str("unsigned int id;\n");
        buffer.push_str("union {\n");

        for (variant_name, variant_types) in &self.variants {
            buffer.push_str(format!("// Variant: {variant_name}\n").as_str());
            buffer.push_str("struct {\n");

            for (index, variant_type) in variant_types.iter().enumerate() {
                buffer.push_str(format!("{variant_type} {variant_name}_{index};\n").as_str());
            }

            buffer.push_str("};\n");
        }

        buffer.push_str("};\n");
        buffer.push_str("};\n");

        buffer
    }

    pub fn build_constructors(&self) -> String {
        let mut buffer = String::new();

        for (variant_id, (variant_name, variant_types)) in self.variants.iter().enumerate() {
            let arguments = variant_types
                .iter()
                .enumerate()
                .map(|(index, variant_type)| format!("{variant_type} {variant_name}_{index}"))
                .collect::<Vec<String>>()
                .join(", ");

            buffer.push_str(
                format!(
                    "{}* _create_{}_{variant_name}({arguments}){{\n",
                    self.name, self.name
                )
                .as_str(),
            );
            buffer.push_str(
                format!("{}* tmp = malloc(sizeof({}));\n", self.name, self.name).as_str(),
            );
            buffer.push_str(format!("tmp->id = {variant_id};\n").as_str());

            for index in 0..variant_types.len() {
                buffer.push_str(
                    format!("tmp->{variant_name}_{index} = {variant_name}_{index};\n").as_str(),
                );
            }

            buffer.push_str("return tmp;\n");
            buffer.push_str("}\n");
        }

        buffer
    }
}
