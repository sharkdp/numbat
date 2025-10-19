use thiserror::Error;

use crate::Statement;
use crate::type_variable::TypeVariable;
use crate::typed_ast::{DType, DTypeFactor, DefineVariable, Expression, StructInfo, Type};

#[derive(Debug, Clone)]
pub struct Substitution(pub Vec<(TypeVariable, Type)>);

impl Substitution {
    pub fn empty() -> Substitution {
        Substitution(vec![])
    }

    pub fn single(v: TypeVariable, t: Type) -> Substitution {
        Substitution(vec![(v, t)])
    }

    pub fn lookup(&self, v: &TypeVariable) -> Option<&Type> {
        self.0.iter().find(|(var, _)| var == v).map(|(_, t)| t)
    }

    // pub fn pretty_print(&self) -> String {
    //     self.0
    //         .iter()
    //         .map(|(v, t)| format!("  {} := {}", v.name(), t))
    //         .collect::<Vec<String>>()
    //         .join("\n")
    // }

    pub fn extend(&mut self, other: Substitution) {
        for (_, t) in &mut self.0 {
            t.apply(&other).unwrap(); // TODO: is the unwrap okay here?
        }
        self.0.extend(other.0);
    }
}

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum SubstitutionError {
    #[error("Used non-dimension type '{0}' in a dimension expression")]
    SubstitutedNonDTypeWithinDType(Type),
}

pub trait ApplySubstitution {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError>;
}

impl ApplySubstitution for Type {
    fn apply(&mut self, s: &Substitution) -> Result<(), SubstitutionError> {
        match self {
            Type::TVar(v) => {
                if let Some(type_) = s.lookup(v) {
                    *self = type_.clone();
                }
                Ok(())
            }
            Type::TPar(n) => {
                if let Some(type_) = s.lookup(&TypeVariable::new(n)) {
                    *self = type_.clone();
                }
                Ok(())
            }
            Type::Dimension(dtype) if dtype.deconstruct_as_single_type_variable().is_some() => {
                let v = dtype.deconstruct_as_single_type_variable().unwrap();
                if let Some(type_) = s.lookup(&v) {
                    *self = type_.clone();
                }
                Ok(())
            }
            Type::Dimension(dtype) => dtype.apply(s),
            Type::Boolean => Ok(()),
            Type::String => Ok(()),
            Type::DateTime => Ok(()),
            Type::Fn(param_types, return_type) => {
                for param_type in param_types {
                    param_type.apply(s)?;
                }
                return_type.apply(s)
            }
            Type::Struct(info) => {
                for (_, field_type) in info.fields.values_mut() {
                    field_type.apply(s)?;
                }
                Ok(())
            }
            Type::List(element_type) => element_type.apply(s),
        }
    }
}

impl ApplySubstitution for DType {
    fn apply(&mut self, substitution: &Substitution) -> Result<(), SubstitutionError> {
        let mut new_dtype = self.clone();
        for (f, power) in self.factors() {
            match f {
                DTypeFactor::TVar(tv) => {
                    if let Some(type_) = substitution.lookup(tv) {
                        let dtype = match type_ {
                            Type::Dimension(dt) => dt.clone(),
                            Type::TVar(tv) => DType::from_type_variable(tv.clone()),
                            t => {
                                return Err(SubstitutionError::SubstitutedNonDTypeWithinDType(
                                    t.clone(),
                                ));
                            }
                        };

                        new_dtype =
                            new_dtype.divide(&DType::from_type_variable(tv.clone()).power(*power));
                        new_dtype = new_dtype.multiply(&dtype.power(*power));
                    }
                }
                DTypeFactor::TPar(name) => {
                    let tv = TypeVariable::new(name);

                    if let Some(type_) = substitution.lookup(&tv) {
                        let dtype = match type_ {
                            Type::Dimension(dt) => dt.clone(),
                            Type::TVar(tv) => DType::from_type_variable(tv.clone()),
                            t => {
                                return Err(SubstitutionError::SubstitutedNonDTypeWithinDType(
                                    t.clone(),
                                ));
                            }
                        };

                        new_dtype = new_dtype
                            .divide(&DType::from_type_parameter(name.clone()).power(*power));
                        new_dtype = new_dtype.multiply(&dtype.power(*power));
                    }
                }
                DTypeFactor::BaseDimension(_) => {}
            }
        }

        *self = new_dtype;
        Ok(())
    }
}

impl ApplySubstitution for StructInfo {
    fn apply(&mut self, s: &Substitution) -> Result<(), SubstitutionError> {
        for (_, field_type) in self.fields.values_mut() {
            field_type.apply(s)?;
        }
        Ok(())
    }
}

impl ApplySubstitution for Expression<'_> {
    fn apply(&mut self, s: &Substitution) -> Result<(), SubstitutionError> {
        match self {
            Expression::Scalar(_, _, type_) => type_.apply(s),
            Expression::Identifier(_, _, type_) => type_.apply(s),
            Expression::UnitIdentifier(_, _, _, _, type_) => type_.apply(s),
            Expression::UnaryOperator(_, _, expr, type_) => {
                expr.apply(s)?;
                type_.apply(s)
            }
            Expression::BinaryOperator(_, _, lhs, rhs, type_) => {
                lhs.apply(s)?;
                rhs.apply(s)?;
                type_.apply(s)
            }
            Expression::BinaryOperatorForDate(_, _, lhs, rhs, type_) => {
                lhs.apply(s)?;
                rhs.apply(s)?;
                type_.apply(s)
            }
            Expression::FunctionCall(_, _, _, arguments, return_type) => {
                for arg in arguments {
                    arg.apply(s)?;
                }
                return_type.apply(s)
            }
            Expression::CallableCall(_, callable, arguments, return_type) => {
                callable.apply(s)?;
                for arg in arguments {
                    arg.apply(s)?;
                }
                return_type.apply(s)
            }
            Expression::Boolean(_, _) => Ok(()),
            Expression::Condition(_, if_, then_, else_) => {
                if_.apply(s)?;
                then_.apply(s)?;
                else_.apply(s)
            }
            Expression::String(_, _) => Ok(()),
            Expression::InstantiateStruct(_, initializers, info) => {
                for (_, expr) in initializers {
                    expr.apply(s)?;
                }
                info.apply(s)
            }
            Expression::AccessField(_, _, instance, _, struct_type, field_type) => {
                instance.apply(s)?;
                struct_type.apply(s)?;
                field_type.apply(s)
            }
            Expression::List(_, elements, element_type) => {
                for element in elements {
                    element.apply(s)?;
                }
                element_type.apply(s)
            }
            Expression::TypedHole(_, type_) => type_.apply(s),
        }
    }
}

impl ApplySubstitution for Statement<'_> {
    fn apply(&mut self, s: &Substitution) -> Result<(), SubstitutionError> {
        match self {
            Statement::Expression(e) => e.apply(s),
            Statement::DefineVariable(DefineVariable(_, _, e, _annotation, type_, _)) => {
                e.apply(s)?;
                type_.apply(s)
            }
            Statement::DefineFunction(_, _, _, _, body, local_variables, fn_type, _, _) => {
                for local_variable in local_variables {
                    local_variable.2.apply(s)?;
                    local_variable.4.apply(s)?;
                }
                if let Some(body) = body {
                    body.apply(s)?;
                }
                fn_type.apply(s)
            }
            Statement::DefineDimension(_, _) => Ok(()),
            Statement::DefineBaseUnit(_, _, _annotation, type_) => type_.apply(s),
            Statement::DefineDerivedUnit(_, _, e, _, _annotation, type_, _) => {
                e.apply(s)?;
                type_.apply(s)
            }
            Statement::ProcedureCall(_, _, args) => {
                for arg in args {
                    arg.apply(s)?;
                }
                Ok(())
            }
            Statement::DefineStruct(info) => {
                info.apply(s)?;

                Ok(())
            }
        }
    }
}
