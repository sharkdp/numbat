use crate::typechecker::type_scheme::TypeScheme;
use crate::typed_ast::{DefineVariable, Expression, Statement, StructInfo};

pub trait ForAllTypeSchemes {
    fn for_all_type_schemes(&mut self, f: &mut dyn FnMut(&mut TypeScheme));
}

impl ForAllTypeSchemes for StructInfo {
    fn for_all_type_schemes(&mut self, _f: &mut dyn FnMut(&mut TypeScheme)) {
        // TODO: once we have generic structs, we also need to traverse into the field types
    }
}

impl ForAllTypeSchemes for Expression<'_> {
    fn for_all_type_schemes(&mut self, f: &mut dyn FnMut(&mut TypeScheme)) {
        match self {
            Expression::Scalar { type_scheme, .. } => f(type_scheme),
            Expression::Identifier { type_scheme, .. } => f(type_scheme),
            Expression::UnitIdentifier { type_scheme, .. } => f(type_scheme),
            Expression::UnaryOperator {
                expr, type_scheme, ..
            } => {
                expr.for_all_type_schemes(f);
                f(type_scheme);
            }
            Expression::BinaryOperator {
                lhs,
                rhs,
                type_scheme,
                ..
            } => {
                lhs.for_all_type_schemes(f);
                rhs.for_all_type_schemes(f);
                f(type_scheme);
            }
            Expression::BinaryOperatorForDate {
                lhs,
                rhs,
                type_scheme,
                ..
            } => {
                lhs.for_all_type_schemes(f);
                rhs.for_all_type_schemes(f);
                f(type_scheme);
            }
            Expression::FunctionCall {
                args, type_scheme, ..
            } => {
                for arg in args {
                    arg.for_all_type_schemes(f);
                }
                f(type_scheme)
            }
            Expression::CallableCall {
                callable,
                args,
                type_scheme,
                ..
            } => {
                callable.for_all_type_schemes(f);
                for arg in args {
                    arg.for_all_type_schemes(f);
                }
                f(type_scheme)
            }
            Expression::Boolean(_, _) => {}
            Expression::Condition {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                condition.for_all_type_schemes(f);
                then_expr.for_all_type_schemes(f);
                else_expr.for_all_type_schemes(f);
            }
            Expression::String(_, _) => {}
            Expression::InstantiateStruct {
                fields,
                struct_info,
                ..
            } => {
                for (_, expr) in fields {
                    expr.for_all_type_schemes(f);
                }
                struct_info.for_all_type_schemes(f);
            }
            Expression::AccessField {
                expr,
                struct_type,
                field_type,
                ..
            } => {
                expr.for_all_type_schemes(f);
                f(struct_type);
                f(field_type);
            }
            Expression::List {
                elements,
                type_scheme,
                ..
            } => {
                for element in elements {
                    element.for_all_type_schemes(f);
                }
                f(type_scheme);
            }
            Expression::TypedHole(_, type_) => {
                f(type_);
            }
        }
    }
}

impl ForAllTypeSchemes for Statement<'_> {
    fn for_all_type_schemes(&mut self, f: &mut dyn FnMut(&mut TypeScheme)) {
        match self {
            Statement::Expression(expr) => expr.for_all_type_schemes(f),
            Statement::DefineVariable(DefineVariable {
                expr, type_scheme, ..
            }) => {
                expr.for_all_type_schemes(f);
                f(type_scheme);
            }
            Statement::DefineFunction {
                body,
                local_variables,
                fn_type,
                ..
            } => {
                for local_variable in local_variables {
                    local_variable.expr.for_all_type_schemes(f);
                    f(&mut local_variable.type_scheme);
                }
                if let Some(body) = body {
                    body.for_all_type_schemes(f);
                }
                f(fn_type);
            }
            Statement::DefineDimension(_, _) => {}
            Statement::DefineBaseUnit { type_scheme, .. } => {
                f(type_scheme);
            }
            Statement::DefineDerivedUnit {
                expr, type_scheme, ..
            } => {
                expr.for_all_type_schemes(f);
                f(type_scheme);
            }
            Statement::ProcedureCall { args, .. } => {
                for arg in args {
                    arg.for_all_type_schemes(f);
                }
            }
            Statement::DefineStruct(info) => info.for_all_type_schemes(f),
        }
    }
}

pub trait ForAllExpressions {
    fn for_all_expressions(&self, f: &mut dyn FnMut(&Expression));
}

impl ForAllExpressions for Statement<'_> {
    fn for_all_expressions(&self, f: &mut dyn FnMut(&Expression)) {
        match self {
            Statement::Expression(expr) => expr.for_all_expressions(f),
            Statement::DefineVariable(DefineVariable { expr, .. }) => expr.for_all_expressions(f),
            Statement::DefineFunction {
                body,
                local_variables,
                ..
            } => {
                for local_variable in local_variables {
                    local_variable.expr.for_all_expressions(f);
                }
                if let Some(body) = body {
                    body.for_all_expressions(f);
                }
            }
            Statement::DefineDimension(_, _) => {}
            Statement::DefineBaseUnit { .. } => {}
            Statement::DefineDerivedUnit { expr, .. } => expr.for_all_expressions(f),
            Statement::ProcedureCall { args, .. } => {
                for arg in args {
                    arg.for_all_expressions(f);
                }
            }
            Statement::DefineStruct(_) => {}
        }
    }
}

impl ForAllExpressions for Expression<'_> {
    fn for_all_expressions(&self, f: &mut dyn FnMut(&Expression)) {
        f(self);
        match self {
            Expression::Scalar { .. } => {}
            Expression::Identifier { .. } => {}
            Expression::UnitIdentifier { .. } => {}
            Expression::UnaryOperator { expr, .. } => expr.for_all_expressions(f),
            Expression::BinaryOperator { lhs, rhs, .. } => {
                lhs.for_all_expressions(f);
                rhs.for_all_expressions(f);
            }
            Expression::BinaryOperatorForDate { lhs, rhs, .. } => {
                lhs.for_all_expressions(f);
                rhs.for_all_expressions(f);
            }
            Expression::FunctionCall { args, .. } => {
                for arg in args {
                    arg.for_all_expressions(f);
                }
            }
            Expression::CallableCall { callable, args, .. } => {
                callable.for_all_expressions(f);
                for arg in args {
                    arg.for_all_expressions(f);
                }
            }
            Expression::Boolean(_, _) => {}
            Expression::Condition {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                condition.for_all_expressions(f);
                then_expr.for_all_expressions(f);
                else_expr.for_all_expressions(f);
            }
            Expression::String(_, _) => {}
            Expression::InstantiateStruct { fields, .. } => {
                for (_, expr) in fields {
                    expr.for_all_expressions(f);
                }
            }
            Expression::AccessField { expr, .. } => {
                expr.for_all_expressions(f);
            }
            Expression::List { elements, .. } => {
                for element in elements {
                    element.for_all_expressions(f);
                }
            }
            Expression::TypedHole(_, _) => {}
        }
    }
}
