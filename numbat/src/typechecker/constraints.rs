use thiserror::Error;

use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use crate::type_variable::TypeVariable;
use crate::typed_ast::{DType, DTypeFactor, Type};

use log::debug;

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum ConstraintSolverError {
    #[error("Could not solve the following constraints:\n{0}")]
    CouldNotSolve(String),
    #[error(transparent)]
    SubstitutionError(SubstitutionError),
}

/// Constraints can be solved trivially, with a new substitution,
/// or by replacing an existing constraint with new constraints.
///
/// For example, the unification constraint `T1 ~ Bool` can be solved with
/// the substitution `T1 := Bool`. The constraint `List<T1> ~ List<T2>`
/// can be solved by replacing it with a `T1 ~ T2` constraint. And the
/// constraint `T0/T1: Dim` can be 'solved' by replacing it with two new
/// constraints `T0: Dim` and `T1: Dim`.
pub struct Satisfied {
    new_substitution: Substitution,
    new_constraints: Vec<Constraint>,
}

impl Satisfied {
    pub fn trivially() -> Self {
        Satisfied {
            new_substitution: Substitution::empty(),
            new_constraints: vec![],
        }
    }

    pub fn with_substitution(s: Substitution) -> Self {
        Satisfied {
            new_substitution: s,
            new_constraints: vec![],
        }
    }

    pub fn with_new_constraints(constraints: Vec<Constraint>) -> Self {
        Satisfied {
            new_substitution: Substitution::empty(),
            new_constraints: constraints,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ConstraintSet {
    constraints: Vec<Constraint>,
}

impl ConstraintSet {
    pub fn add(&mut self, constraint: Constraint) -> TrivialResultion {
        let result = constraint.try_trivial_resolution();

        match result {
            TrivialResultion::Satisfied => {}
            TrivialResultion::Violated => {
                self.constraints.push(constraint);
            }
            TrivialResultion::Unknown => {
                self.constraints.push(constraint);
            }
        }

        result
    }

    // pub fn extend(&mut self, other: ConstraintSet) {
    //     self.constraints.extend(other.constraints);
    // }

    pub fn solve(&mut self) -> Result<(Substitution, Vec<TypeVariable>), ConstraintSolverError> {
        let mut substitution = Substitution::empty();

        let mut made_progress = true;
        while made_progress {
            made_progress = false;
            let mut new_constraint_set = self.clone();

            for (i, c) in self.iter().enumerate() {
                match c.try_satisfy() {
                    Some(Satisfied {
                        new_constraints,
                        new_substitution,
                    }) => {
                        new_constraint_set.remove(i);
                        new_constraint_set.constraints.extend(new_constraints);

                        new_constraint_set
                            .apply(&new_substitution)
                            .map_err(ConstraintSolverError::SubstitutionError)?;

                        substitution.extend(new_substitution);

                        debug!(
                            "    New constraints:\n{}",
                            new_constraint_set.pretty_print(6)
                        );

                        made_progress = true;
                        break;
                    }
                    None => {}
                }
            }

            self.constraints = new_constraint_set.constraints;
        }

        // Solve remaining type class constraints (if possible), by remembering
        // `T_i: Dim` bounds for those type variables
        let mut dtypes = vec![];
        let mut remaining_constraints = vec![];
        for c in self.iter() {
            match c.get_dtype_constraint_type_variable() {
                None => {
                    remaining_constraints.push(c.clone());
                }
                Some(name) => {
                    dtypes.push(name);
                }
            }
        }
        dtypes.sort();
        dtypes.dedup();

        if !remaining_constraints.is_empty() {
            return Err(ConstraintSolverError::CouldNotSolve(
                remaining_constraints
                    .iter()
                    .map(|c| c.pretty_print())
                    .collect::<Vec<String>>()
                    .join("\n"),
            ));
        }

        Ok((substitution, dtypes))
    }

    fn remove(&mut self, i: usize) {
        self.constraints.remove(i);
    }

    pub fn iter(&self) -> impl Iterator<Item = &Constraint> {
        self.constraints.iter()
    }

    pub fn pretty_print(&self, indent: usize) -> String {
        self.constraints
            .iter()
            .map(|c| format!("{:indent$}{}", "", c.pretty_print(), indent = indent))
            .collect::<Vec<String>>()
            .join("\n")
    }
}

impl ApplySubstitution for ConstraintSet {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError> {
        for c in self.constraints.iter_mut() {
            c.apply(substitution)?;
        }
        Ok(())
    }
}

/// When we add new constraints, we check whether they can be trivially resolved to
/// either true or false
#[derive(Debug, Clone, PartialEq)]
#[must_use]
pub enum TrivialResultion {
    Satisfied,
    Violated,
    Unknown,
}

impl TrivialResultion {
    pub fn is_trivially_violated(self) -> bool {
        matches!(self, TrivialResultion::Violated)
    }

    /// Ignore the result of the trivial resolution. This is a helper to prevent the
    /// `must_use` attribute from being triggered.
    pub(crate) fn ok(&self) -> () {}
}

/// A type checker constraint can be one of three things:
/// - A unification constraint `Type1 ~ Type2` which constrains two types to be equal
/// - A 'type class' constraint `Type: DType` which constrains `Type` to be a dimension type (like `Scalar`, `Length`, or `Length × Mass / Time²`).
/// - A constraint `DType ~ Scalar` which constrains a dimension type to be dimensionless.
#[derive(Clone, Debug, PartialEq)]
pub enum Constraint {
    Equal(Type, Type),
    IsDType(Type),
    EqualScalar(DType),
}

impl Constraint {
    fn try_trivial_resolution(&self) -> TrivialResultion {
        match self {
            Constraint::Equal(t1, t2)
                if t1.type_variables().is_empty() && t2.type_variables().is_empty() =>
            {
                if t1 == t2 {
                    TrivialResultion::Satisfied
                } else {
                    TrivialResultion::Violated
                }
            }
            Constraint::Equal(_, _) => TrivialResultion::Unknown,
            Constraint::IsDType(t) if t.type_variables().is_empty() => match t {
                Type::Dimension(_) => TrivialResultion::Satisfied,
                _ => TrivialResultion::Violated,
            },
            Constraint::IsDType(_) => TrivialResultion::Unknown,
            Constraint::EqualScalar(d) if d.is_scalar() => TrivialResultion::Satisfied,
            Constraint::EqualScalar(d) if d.type_variables().is_empty() => {
                TrivialResultion::Violated
            }
            Constraint::EqualScalar(_) => TrivialResultion::Unknown,
        }
    }

    /// Try to solve a constraint. Returns `None` if the constaint can not (yet) be solved.
    fn try_satisfy(&self) -> Option<Satisfied> {
        match self {
            Constraint::Equal(t1, t2) if t1 == t2 => {
                debug!("  (1) SOLVING: {} ~ {} trivially ", t1, t2);
                Some(Satisfied::trivially())
            }
            Constraint::Equal(Type::TVar(x), t) if !t.contains(x) => {
                debug!(
                    "  (2) SOLVING: {x} ~ {t} with substitution {x} := {t}",
                    x = x.name(),
                    t = t
                );
                Some(Satisfied::with_substitution(Substitution::single(
                    x.clone(),
                    t.clone(),
                )))
            }
            Constraint::Equal(s, Type::TVar(x)) if !s.contains(x) => {
                debug!(
                    "  (3) SOLVING: {s} ~ {x} with substitution {x} := {s}",
                    s = s,
                    x = x.name()
                );
                Some(Satisfied::with_substitution(Substitution::single(
                    x.clone(),
                    s.clone(),
                )))
            }
            // Constraint::Equal(t @ Type::TArr(s1, s2), s @ Type::TArr(t1, t2)) => {
            //     debug!(
            //         "  (4) SOLVING: {t} ~ {s} with new constraints {s1} ~ {t1} and {s2} ~ {t2}",
            //         t = t,
            //         s = s,
            //         s1 = s1,
            //         s2 = s2,
            //         t1 = t1,
            //         t2 = t2
            //     );
            //     Some(Satisfied::with_new_constraints(vec![
            //         Constraint::Equal(s1.as_ref().clone(), t1.as_ref().clone()),
            //         Constraint::Equal(s2.as_ref().clone(), t2.as_ref().clone()),
            //     ]))
            // }
            Constraint::Equal(s @ Type::List(s1), t @ Type::List(t1)) => {
                debug!(
                    "  (5) SOLVING: {s} ~ {t} with new constraint {s1} ~ {t1}",
                    s = s,
                    t = t,
                    s1 = s1,
                    t1 = t1
                );
                Some(Satisfied::with_new_constraints(vec![Constraint::Equal(
                    s1.as_ref().clone(),
                    t1.as_ref().clone(),
                )]))
            }
            Constraint::Equal(Type::TVar(tv), Type::Dimension(d))
            | Constraint::Equal(Type::Dimension(d), Type::TVar(tv)) => {
                debug!(
                    "  (6) SOLVING: {tv} ~ {d} by lifting the type variable to a DType",
                    tv = tv.name(),
                    d = d
                );

                Some(Satisfied::with_new_constraints(vec![Constraint::Equal(
                    Type::Dimension(DType::from_type_variable(tv.clone())),
                    Type::Dimension(d.clone()),
                )]))
            }
            Constraint::Equal(Type::Dimension(d1), Type::Dimension(d2)) => {
                let d_result = d1.divide(d2);
                debug!(
                    "  (7) SOLVING: {} ~ {} with new constraint d_result = Scalar",
                    d1.pretty_print(),
                    d2.pretty_print()
                );
                Some(Satisfied::with_new_constraints(vec![
                    Constraint::EqualScalar(d_result),
                ]))
            }
            Constraint::Equal(_, _) => None,
            Constraint::IsDType(Type::Dimension(inner)) => {
                let new_constraints = inner
                    .type_variables()
                    .iter()
                    .map(|tv| Constraint::IsDType(Type::TVar(tv.clone())))
                    .collect();
                debug!(
                    "  (8) SOLVING: {} : DType through new constraints: {:?}",
                    inner.pretty_print(),
                    new_constraints
                );
                Some(Satisfied::with_new_constraints(new_constraints))
            }
            Constraint::IsDType(_) => None,
            Constraint::EqualScalar(d) if d == &DType::scalar() => {
                debug!("  (9) SOLVING: Scalar = Scalar trivially");
                Some(Satisfied::trivially())
            }
            Constraint::EqualScalar(dtype) => match dtype.split_first_factor() {
                Some(((DTypeFactor::TVar(tv), k), rest)) => {
                    let result = DType::from_factors(
                        &rest
                            .iter()
                            .map(|(f, j)| (f.clone(), -j / k))
                            .collect::<Vec<_>>(),
                    );
                    debug!(
                        "  (10) SOLVING: {dtype} = Scalar with substitution {tv} := {result}",
                        dtype = dtype.pretty_print(),
                        tv = tv.name(),
                        result = result.pretty_print()
                    );
                    Some(Satisfied::with_substitution(Substitution::single(
                        tv.clone(),
                        Type::Dimension(result),
                    )))
                }
                _ => None,
            },
        }
    }

    fn pretty_print(&self) -> String {
        match self {
            Constraint::Equal(t1, t2) => {
                format!("  {} ~ {}", t1, t2)
            }
            Constraint::IsDType(t) => format!("  {}: DType", t),
            Constraint::EqualScalar(d) => format!("  {} = Scalar", d),
        }
    }

    // Get the contained type variable, if this constraint is a trivial dtype constraint for a type variable
    fn get_dtype_constraint_type_variable(&self) -> Option<TypeVariable> {
        match self {
            Constraint::IsDType(Type::TVar(tvar)) => Some(tvar.clone()),
            _ => None,
        }
    }
}

impl ApplySubstitution for Constraint {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError> {
        match self {
            Constraint::Equal(t1, t2) => {
                t1.apply(substitution)?;
                t2.apply(substitution)?;
            }
            Constraint::IsDType(t) => {
                t.apply(substitution)?;
            }
            Constraint::EqualScalar(d) => d.apply(substitution)?,
        }
        Ok(())
    }
}
