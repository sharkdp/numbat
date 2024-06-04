use crate::typechecker::type_scheme::TypeScheme;
use crate::typed_ast::{Expression, Statement, StructInfo};

pub trait ForAllTypeSchemes {
    fn for_all_type_schemes(&mut self, f: &mut dyn FnMut(&mut TypeScheme));
}

impl ForAllTypeSchemes for StructInfo {
    fn for_all_type_schemes(&mut self, _f: &mut dyn FnMut(&mut TypeScheme)) {
        // TODO: once we have generic structs, we also need to traverse into the field types
    }
}

impl ForAllTypeSchemes for Expression {
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
            Expression::AccessField(_, _, expr, _, info, type_) => {
                expr.for_all_type_schemes(f);
                info.for_all_type_schemes(f);
                f(type_);
            }
            Expression::List(_, elements, type_) => {
                for element in elements {
                    element.for_all_type_schemes(f);
                }
                f(type_);
            }
        }
    }
}

impl ForAllTypeSchemes for Statement {
    fn for_all_type_schemes(&mut self, f: &mut dyn FnMut(&mut TypeScheme)) {
        match self {
            Statement::Expression(expr) => expr.for_all_type_schemes(f),
            Statement::DefineVariable(_, _, expr, type_) => {
                expr.for_all_type_schemes(f);
                f(type_);
            }
            Statement::DefineFunction(_, _, _, _, body, fn_type) => {
                if let Some(body) = body {
                    body.for_all_type_schemes(f);
                }
                f(fn_type);
            }
            Statement::DefineDimension(_, _) => {}
            Statement::DefineBaseUnit(_, _, _annotation, type_) => {
                f(type_);
            }
            Statement::DefineDerivedUnit(_, expr, _, _annotation, type_) => {
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
