use compact_str::CompactString;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeVariable {
    Named(CompactString),
    Quantified(usize),
}

impl TypeVariable {
    pub fn new<S: AsRef<str>>(name: S) -> TypeVariable {
        TypeVariable::Named(name.as_ref().into())
    }

    pub fn new_gen(i: usize) -> TypeVariable {
        TypeVariable::Quantified(i)
    }

    pub fn unsafe_name(&self) -> &str {
        if let TypeVariable::Named(name) = self {
            name
        } else {
            unreachable!("Expected type variable to be named");
        }
    }
}
