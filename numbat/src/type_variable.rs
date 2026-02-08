use compact_str::CompactString;

/// Represents a type variable, used both during inference and in generalized type schemes.
///
/// - `Named`: Fresh unification variables generated during type inference (`T0`, `T1`, ...).
///   These are created for unannotated parameters and intermediate expressions.
/// - `Quantified`: De Bruijn-style index into the bound variables of a `TypeScheme`.
///   Example: in `forall A. forall B. A -> B`, `A` is `Quantified(0)` and `B` is `Quantified(1)`.
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
