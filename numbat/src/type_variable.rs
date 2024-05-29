#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeVariable {
    Named(String),
    Quantified(usize),
}

impl TypeVariable {
    pub fn new<S: AsRef<str>>(name: S) -> TypeVariable {
        TypeVariable::Named(name.as_ref().into())
    }

    pub fn new_gen(i: usize) -> TypeVariable {
        TypeVariable::Quantified(i)
    }

    pub fn name(&self) -> String {
        match &self {
            TypeVariable::Named(name) => name.into(),
            TypeVariable::Quantified(i) => format!("$tgen_{}", i),
        }
    }
}
