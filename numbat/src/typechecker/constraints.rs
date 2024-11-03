use std::sync::Arc;

use compact_str::{format_compact, CompactString};

use super::substitutions::{ApplySubstitution, Substitution, SubstitutionError};
use crate::type_variable::TypeVariable;
use crate::typed_ast::{DType, DTypeFactor, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintSolverError {
    CouldNotSolve(String),
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
    pub fn add(&mut self, constraint: Constraint) -> TrivialResolution {
        let result = constraint.try_trivial_resolution();

        match result {
            TrivialResolution::Satisfied => {}
            TrivialResolution::Violated => {
                self.constraints.push(constraint);
            }
            TrivialResolution::Unknown => {
                self.constraints.push(constraint);
            }
        }

        result
    }

    pub(crate) fn add_equal_constraint(&mut self, lhs: &Type, rhs: &Type) -> TrivialResolution {
        self.add(Constraint::Equal(lhs.clone(), rhs.clone()))
    }

    pub(crate) fn add_dtype_constraint(&mut self, type_: &Type) -> TrivialResolution {
        self.add(Constraint::IsDType(type_.clone()))
    }

    pub fn clear(&mut self) {
        self.constraints.clear();
    }

    pub fn solve(&mut self) -> Result<(Substitution, Vec<TypeVariable>), ConstraintSolverError> {
        let mut substitution = Substitution::empty();

        let mut made_progress = true;
        while made_progress {
            made_progress = false;
            let mut new_constraint_set = self.clone();

            for (i, c) in self.iter().enumerate() {
                if let Some(Satisfied {
                    new_constraints,
                    new_substitution,
                }) = c.try_satisfy()
                {
                    new_constraint_set.remove(i);
                    new_constraint_set.constraints.extend(new_constraints);

                    new_constraint_set
                        .apply(&new_substitution)
                        .map_err(ConstraintSolverError::SubstitutionError)?;

                    substitution.extend(new_substitution);

                    made_progress = true;
                    break;
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
                    .collect::<Vec<CompactString>>()
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
pub enum TrivialResolution {
    Satisfied,
    Violated,
    Unknown,
}

impl TrivialResolution {
    #[allow(clippy::wrong_self_convention)]
    pub fn is_trivially_violated(self) -> bool {
        matches!(self, TrivialResolution::Violated)
    }

    /// Ignore the result of the trivial resolution. This is a helper to prevent the
    /// `must_use` attribute from being triggered.
    pub(crate) fn ok(&self) {}
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
    HasField(Type, CompactString, Type),
}

impl Constraint {
    fn try_trivial_resolution(&self) -> TrivialResolution {
        match self {
            Constraint::Equal(t1, t2) if t1.is_closed() && t2.is_closed() => {
                if t1 == t2 {
                    TrivialResolution::Satisfied
                } else {
                    TrivialResolution::Violated
                }
            }
            Constraint::Equal(Type::Fn(params1, _), Type::Fn(params2, _))
                if params1.len() != params2.len() =>
            {
                TrivialResolution::Violated
            }
            Constraint::Equal(_, _) => TrivialResolution::Unknown,
            Constraint::IsDType(t) if t.is_closed() => match t {
                Type::Dimension(_) => TrivialResolution::Satisfied,
                _ => TrivialResolution::Violated,
            },
            Constraint::IsDType(_) => TrivialResolution::Unknown,
            Constraint::EqualScalar(d) if d.is_scalar() => TrivialResolution::Satisfied,
            Constraint::EqualScalar(d) if d.type_variables(false).is_empty() => {
                TrivialResolution::Violated
            }
            Constraint::EqualScalar(_) => TrivialResolution::Unknown,
            Constraint::HasField(_, _, _) => {
                // Trivial resolution handling for structs is done directly in the type checker
                TrivialResolution::Unknown
            }
        }
    }

    /// Try to solve a constraint. Returns `None` if the constaint can not (yet) be solved.
    fn try_satisfy(&self) -> Option<Satisfied> {
        match self {
            Constraint::Equal(t1, t2) if t1 == t2 => Some(Satisfied::trivially()),
            Constraint::Equal(Type::TVar(x), t) | Constraint::Equal(t, Type::TVar(x))
                if !t.contains(x, false) =>
            {
                Some(Satisfied::with_substitution(Substitution::single(
                    x.clone(),
                    t.clone(),
                )))
            }
            Constraint::Equal(Type::Dimension(dtype_x), t)
                if dtype_x
                    .deconstruct_as_single_type_variable()
                    .map(|tv| !t.contains(&tv, false))
                    .unwrap_or(false) =>
            {
                let x = dtype_x.deconstruct_as_single_type_variable().unwrap();
                Some(Satisfied::with_substitution(Substitution::single(
                    x.clone(),
                    t.clone(),
                )))
            }
            Constraint::Equal(t, Type::Dimension(dtype_x))
                if dtype_x
                    .deconstruct_as_single_type_variable()
                    .map(|tv| !t.contains(&tv, false))
                    .unwrap_or(false) =>
            {
                let x = dtype_x.deconstruct_as_single_type_variable().unwrap();
                Some(Satisfied::with_substitution(Substitution::single(
                    x.clone(),
                    t.clone(),
                )))
            }
            Constraint::Equal(Type::Fn(params1, return1), Type::Fn(params2, return2))
                if params1.len() == params2.len() =>
            {
                let mut new_constraints = vec![Constraint::Equal(
                    return1.as_ref().clone(),
                    return2.as_ref().clone(),
                )];
                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    new_constraints.push(Constraint::Equal(p1.clone(), p2.clone()));
                }

                Some(Satisfied::with_new_constraints(new_constraints))
            }
            Constraint::Equal(Type::List(s1), Type::List(t1)) => {
                Some(Satisfied::with_new_constraints(vec![Constraint::Equal(
                    s1.as_ref().clone(),
                    t1.as_ref().clone(),
                )]))
            }
            Constraint::Equal(Type::TVar(tv), Type::Dimension(d))
            | Constraint::Equal(Type::Dimension(d), Type::TVar(tv)) => {
                Some(Satisfied::with_new_constraints(vec![Constraint::Equal(
                    Type::Dimension(DType::from_type_variable(tv.clone())),
                    Type::Dimension(d.clone()),
                )]))
            }
            Constraint::Equal(Type::Dimension(d1), Type::Dimension(d2)) => {
                let d_result = d1.divide(d2);
                Some(Satisfied::with_new_constraints(vec![
                    Constraint::EqualScalar(d_result),
                ]))
            }
            Constraint::Equal(_, _) => None,
            Constraint::IsDType(Type::Dimension(inner)) => {
                let new_constraints = inner
                    .type_variables(true)
                    .iter()
                    .map(|tv| Constraint::IsDType(Type::TVar(tv.clone())))
                    .collect();
                Some(Satisfied::with_new_constraints(new_constraints))
            }
            Constraint::IsDType(_) => None,
            Constraint::EqualScalar(d) if d == &DType::scalar() => Some(Satisfied::trivially()),
            Constraint::EqualScalar(dtype) => match dtype.split_first_factor() {
                Some(((DTypeFactor::TVar(tv), k), rest)) => {
                    let result = DType::from_factors(Arc::new(
                        rest.iter().map(|(f, j)| (f.clone(), -j / k)).collect(),
                    ));
                    Some(Satisfied::with_substitution(Substitution::single(
                        tv.clone(),
                        Type::Dimension(result),
                    )))
                }
                _ => None,
            },
            Constraint::HasField(struct_type, field_name, field_type)
                if struct_type.is_closed() =>
            {
                if let Type::Struct(info) = struct_type {
                    if let Some((_, actual_field_type)) = info.fields.get(field_name) {
                        Some(Satisfied::with_new_constraints(vec![Constraint::Equal(
                            actual_field_type.clone(),
                            field_type.clone(),
                        )]))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Constraint::HasField(_, _, _) => None,
        }
    }

    fn pretty_print(&self) -> CompactString {
        match self {
            Constraint::Equal(t1, t2) => {
                format_compact!("  {t1} ~ {t2}")
            }
            Constraint::IsDType(t) => format_compact!("  {t}: DType"),
            Constraint::EqualScalar(d) => format_compact!("  {d} = Scalar"),
            Constraint::HasField(struct_type, field_name, field_type) => {
                format_compact!("HasField({struct_type}, \"{field_name}\", {field_type})")
            }
        }
    }

    // Get the contained type variable, if this constraint is a trivial dtype constraint for a type variable
    fn get_dtype_constraint_type_variable(&self) -> Option<TypeVariable> {
        match self {
            Constraint::IsDType(Type::TVar(tvar)) => Some(tvar.clone()),
            Constraint::IsDType(Type::TPar(name)) => Some(TypeVariable::new(name.clone())),
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
            Constraint::HasField(struct_type, _, field_type) => {
                struct_type.apply(substitution)?;
                field_type.apply(substitution)?;
            }
        }
        Ok(())
    }
}
