pub struct StructBuilder {
    name: String,
    fields: Vec<(String, String)>,
}

pub struct FunctionBuilder {
    name: String,
    return_type: String,
    parameters: Vec<(String, String)>,
}

impl StructBuilder {
    pub fn new(name: String) -> Self {
        StructBuilder {
            name,
            fields: Vec::new(),
        }
    }

    pub fn field(&mut self, field_name: String, field_type: String) -> &mut StructBuilder {
        self.fields.push((field_name, field_type));

        self
    }

    pub fn build(&self) -> String {
        let body: Vec<String> = self
            .fields
            .iter()
            .map(|(field_name, field_type)| format!("{} {};", field_type, field_name))
            .collect();

        format!(
            "typedef struct {{\n\t{}\n}} {};",
            body.join("\n\t"),
            self.name
        )
    }

    pub fn build_constructor(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(
            format!(
                "\n{}* _create_{}({}){{\n",
                self.name,
                self.name,
                self.fields
                    .iter()
                    .map(|(field_name, _)| format!("{} {}", "int".to_string(), field_name))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
            .as_str(),
        );

        buffer.push_str(format!("{}* tmp = malloc(sizeof({}));\n", self.name, self.name).as_str());

        for (field_name, _) in &self.fields {
            buffer.push_str(format!("(*tmp).{} = {};\n", field_name, field_name).as_str());
        }

        buffer.push_str("return tmp;\n}\n");

        buffer
    }
}
