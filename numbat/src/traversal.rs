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
            Expression::Scalar(_, _, type_) => f(type_),
            Expression::Identifier(_, _, type_) => f(type_),
            Expression::UnitIdentifier(_, _, _, _, type_) => f(type_),
            Expression::UnaryOperator(_, _, expr, type_) => {
                expr.for_all_type_schemes(f);
                f(type_);
            }
            Expression::BinaryOperator(_, _, lhs, rhs, type_) => {
                lhs.for_all_type_schemes(f);
                rhs.for_all_type_schemes(f);
                f(type_);
            }
            Expression::BinaryOperatorForDate(_, _, lhs, rhs, type_) => {
                lhs.for_all_type_schemes(f);
                rhs.for_all_type_schemes(f);
                f(type_);
            }
            Expression::FunctionCall(_, _, _, args, type_) => {
                for arg in args {
                    arg.for_all_type_schemes(f);
                }
                f(type_)
            }
            Expression::CallableCall(_, callable, args, type_) => {
                callable.for_all_type_schemes(f);
                for arg in args {
                    arg.for_all_type_schemes(f);
                }
                f(type_)
            }
            Expression::Boolean(_, _) => {}
            Expression::Condition(_, if_, then_, else_) => {
                if_.for_all_type_schemes(f);
                then_.for_all_type_schemes(f);
                else_.for_all_type_schemes(f);
            }
            Expression::String(_, _) => {}
            Expression::InstantiateStruct(_, initializers, info) => {
                for (_, expr) in initializers {
                    expr.for_all_type_schemes(f);
                }
                info.for_all_type_schemes(f);
            }
            Expression::AccessField(_, _, expr, _, struct_type, field_type) => {
                expr.for_all_type_schemes(f);
                f(struct_type);
                f(field_type);
            }
            Expression::List(_, elements, type_) => {
                for element in elements {
                    element.for_all_type_schemes(f);
                }
                f(type_);
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
            Statement::DefineVariable(DefineVariable(_, _, expr, _annotation, type_, _)) => {
                expr.for_all_type_schemes(f);
                f(type_);
            }
            Statement::DefineFunction(_, _, _, _, body, local_variables, fn_type, _, _) => {
                for local_variable in local_variables {
                    local_variable.2.for_all_type_schemes(f);
                    f(&mut local_variable.4);
                }
                if let Some(body) = body {
                    body.for_all_type_schemes(f);
                }
                f(fn_type);
            }
            Statement::DefineDimension(_, _) => {}
            Statement::DefineBaseUnit(_, _, _annotation, type_) => {
                f(type_);
            }
            Statement::DefineDerivedUnit(_, expr, _, _annotation, type_, _) => {
                expr.for_all_type_schemes(f);
                f(type_);
            }
            Statement::ProcedureCall(_, args) => {
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
            Statement::DefineVariable(DefineVariable(_, _, expr, _, _, _)) => {
                expr.for_all_expressions(f)
            }
            Statement::DefineFunction(_, _, _, _, body, local_variables, _, _, _) => {
                for local_variable in local_variables {
                    local_variable.2.for_all_expressions(f);
                }
                if let Some(body) = body {
                    body.for_all_expressions(f);
                }
            }
            Statement::DefineDimension(_, _) => {}
            Statement::DefineBaseUnit(_, _, _, _) => {}
            Statement::DefineDerivedUnit(_, expr, _, _, _, _) => expr.for_all_expressions(f),
            Statement::ProcedureCall(_, args) => {
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
            Expression::Scalar(_, _, _) => {}
            Expression::Identifier(_, _, _) => {}
            Expression::UnitIdentifier(_, _, _, _, _) => {}
            Expression::UnaryOperator(_, _, expr, _) => expr.for_all_expressions(f),
            Expression::BinaryOperator(_, _, lhs, rhs, _) => {
                lhs.for_all_expressions(f);
                rhs.for_all_expressions(f);
            }
            Expression::BinaryOperatorForDate(_, _, lhs, rhs, _) => {
                lhs.for_all_expressions(f);
                rhs.for_all_expressions(f);
            }
            Expression::FunctionCall(_, _, _, args, _) => {
                for arg in args {
                    arg.for_all_expressions(f);
                }
            }
            Expression::CallableCall(_, callable, args, _) => {
                callable.for_all_expressions(f);
                for arg in args {
                    arg.for_all_expressions(f);
                }
            }
            Expression::Boolean(_, _) => {}
            Expression::Condition(_, if_, then_, else_) => {
                if_.for_all_expressions(f);
                then_.for_all_expressions(f);
                else_.for_all_expressions(f);
            }
            Expression::String(_, _) => {}
            Expression::InstantiateStruct(_, initializers, _) => {
                for (_, expr) in initializers {
                    expr.for_all_expressions(f);
                }
            }
            Expression::AccessField(_, _, expr, _, _, _) => {
                expr.for_all_expressions(f);
            }
            Expression::List(_, elements, _) => {
                for element in elements {
                    element.for_all_expressions(f);
                }
            }
            Expression::TypedHole(_, _) => {}
        }
    }
}
