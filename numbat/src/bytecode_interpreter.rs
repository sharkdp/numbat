use std::collections::HashMap;

use compact_str::{CompactString, ToCompactString};
use itertools::Itertools;

use crate::ast::ProcedureKind;
use crate::decorator::Decorator;
use crate::dimension::DimensionRegistry;
use crate::interpreter::{
    Interpreter, InterpreterResult, InterpreterSettings, Result, RuntimeError,
};
use crate::name_resolution::LAST_RESULT_IDENTIFIERS;
use crate::prefix::Prefix;
use crate::prefix_parser::AcceptsPrefix;
use crate::pretty_print::PrettyPrint;
use crate::typed_ast::{
    BinaryOperator, DefineVariable, Expression, Statement, StringPart, UnaryOperator,
};
use crate::unit::{CanonicalName, Unit};
use crate::unit_registry::{UnitMetadata, UnitRegistry};
use crate::value::{FunctionReference, Value};
use crate::vm::{Constant, ExecutionContext, Op, Vm};
use crate::{decorator, ffi, Type};

#[derive(Debug, Clone, Default)]
pub struct LocalMetadata {
    pub name: Option<CompactString>,
    pub url: Option<CompactString>,
    pub description: Option<CompactString>,
    pub aliases: Vec<CompactString>,
}

#[derive(Debug, Clone)]
pub struct Local {
    identifier: CompactString,
    depth: usize,
    pub metadata: LocalMetadata,
}

#[derive(Clone)]
pub struct BytecodeInterpreter {
    vm: Vm,
    /// List of local variables currently in scope, one vector for each scope (for now: 0: 'global' scope, 1: function scope)
    locals: Vec<Vec<Local>>,
    // Maps names of units to indices of the respective constants in the VM
    unit_name_to_constant_index: HashMap<CompactString, u16>,
    /// List of functions
    functions: HashMap<CompactString, bool>,
}

impl BytecodeInterpreter {
    fn compile_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Scalar(_span, n, _type) => {
                let index = self.vm.add_constant(Constant::Scalar(n.to_f64()));
                self.vm.add_op1(Op::LoadConstant, index);
            }
            Expression::Identifier(_span, identifier, _type) => {
                // Searching in reverse order ensures that we find the innermost identifier of that name first (shadowing)

                let current_depth = self.locals.len() - 1;

                if let Some(position) = self.locals[current_depth]
                    .iter()
                    .rposition(|l| l.identifier == identifier && l.depth == current_depth)
                {
                    self.vm.add_op1(Op::GetLocal, position as u16); // TODO: check overflow
                } else if let Some(upvalue_position) = self.locals[0]
                    .iter()
                    .rposition(|l| l.identifier == identifier)
                {
                    self.vm.add_op1(Op::GetUpvalue, upvalue_position as u16);
                } else if LAST_RESULT_IDENTIFIERS.contains(identifier) {
                    self.vm.add_op(Op::GetLastResult);
                } else if let Some(is_foreign) = self.functions.get(*identifier) {
                    let index = self
                        .vm
                        .add_constant(Constant::FunctionReference(if *is_foreign {
                            FunctionReference::Foreign(identifier.to_compact_string())
                        } else {
                            FunctionReference::Normal(identifier.to_compact_string())
                        }));
                    self.vm.add_op1(Op::LoadConstant, index);
                } else {
                    unreachable!("Unknown identifier '{identifier}'")
                }
            }
            Expression::UnitIdentifier(_span, prefix, unit_name, _full_name, _type) => {
                let index = self
                    .unit_name_to_constant_index
                    .get(unit_name)
                    .expect("unit should already exist");

                self.vm.add_op1(Op::LoadConstant, *index);

                if prefix != &Prefix::none() {
                    let prefix_idx = self.vm.add_prefix(*prefix);
                    self.vm.add_op1(Op::ApplyPrefix, prefix_idx);
                }
            }
            Expression::UnaryOperator(_span, UnaryOperator::Negate, rhs, _type) => {
                self.compile_expression(rhs)?;
                self.vm.add_op(Op::Negate);
            }
            Expression::UnaryOperator(_span, UnaryOperator::Factorial(order), lhs, _type) => {
                self.compile_expression(lhs)?;
                self.vm.add_op1(Op::Factorial, order.get() as u16);
            }
            Expression::UnaryOperator(_span, UnaryOperator::LogicalNeg, lhs, _type) => {
                self.compile_expression(lhs)?;
                self.vm.add_op(Op::LogicalNeg);
            }
            Expression::BinaryOperator(_span, operator, lhs, rhs, _type) => {
                self.compile_expression(lhs)?;
                self.compile_expression(rhs)?;

                let op = match operator {
                    BinaryOperator::Add => Op::Add,
                    BinaryOperator::Sub => Op::Subtract,
                    BinaryOperator::Mul => Op::Multiply,
                    BinaryOperator::Div => Op::Divide,
                    BinaryOperator::Power => Op::Power,
                    BinaryOperator::ConvertTo => Op::ConvertTo,
                    BinaryOperator::LessThan => Op::LessThan,
                    BinaryOperator::GreaterThan => Op::GreaterThan,
                    BinaryOperator::LessOrEqual => Op::LessOrEqual,
                    BinaryOperator::GreaterOrEqual => Op::GreatorOrEqual,
                    BinaryOperator::Equal => Op::Equal,
                    BinaryOperator::NotEqual => Op::NotEqual,
                    BinaryOperator::LogicalAnd => Op::LogicalAnd,
                    BinaryOperator::LogicalOr => Op::LogicalOr,
                };
                self.vm.add_op(op);
            }
            Expression::BinaryOperatorForDate(_span, operator, lhs, rhs, type_) => {
                self.compile_expression(lhs)?;
                self.compile_expression(rhs)?;

                // if the result is a duration:
                let op = if type_.is_dtype() {
                    // the VM will need to return a value with the units of Seconds.  so look up that unit here, and push it
                    // onto the stack, so the VM can easily reference it.
                    // TODO: We do not want to hard-code 'second' here. Instead, we might
                    // introduce a decorator to register the 'second' unit in the prelude for
                    // this specific purpose. We also need to handle errors in case no such unit
                    // was registered.
                    let second_idx = self.unit_name_to_constant_index.get("second");
                    self.vm.add_op1(Op::LoadConstant, *second_idx.unwrap());
                    Op::DiffDateTime
                } else {
                    match operator {
                        BinaryOperator::Add => Op::AddToDateTime,
                        BinaryOperator::Sub => Op::SubFromDateTime,
                        _ => unreachable!("{operator:?} is not valid with a DateTime"), // should be unreachable, because the typechecker will error first
                    }
                };

                self.vm.add_op(op);
            }
            Expression::FunctionCall(_span, _full_span, name, args, _type) => {
                // Put all arguments on top of the stack
                for arg in args {
                    self.compile_expression(arg)?;
                }

                if let Some(idx) = self.vm.get_ffi_callable_idx(name) {
                    // TODO: check overflow:
                    self.vm.add_op2(Op::FFICallFunction, idx, args.len() as u16);
                } else {
                    let idx = self.vm.get_function_idx(name);

                    self.vm.add_op2(Op::Call, idx, args.len() as u16); // TODO: check overflow
                }
            }
            Expression::InstantiateStruct(_span, exprs, struct_info) => {
                // structs must be consistently ordered in the VM, so we reorder
                // the field values so that they are evaluated in the order the
                // struct fields are defined.

                let sorted_exprs = exprs
                    .iter()
                    .sorted_by_key(|(n, _)| struct_info.fields.get_index_of(*n).unwrap());

                for (_, expr) in sorted_exprs.rev() {
                    self.compile_expression(expr)?;
                }

                let struct_info_idx = self.vm.get_structinfo_idx(&struct_info.name).unwrap() as u16;

                self.vm
                    .add_op2(Op::BuildStructInstance, struct_info_idx, exprs.len() as u16);
            }
            Expression::AccessField(_span, _full_span, expr, attr, struct_type, _result_type) => {
                self.compile_expression(expr)?;

                let Type::Struct(ref struct_info) = struct_type.to_concrete_type() else {
                    unreachable!(
                        "Field access of non-struct type should be prevented by the type checker"
                    );
                };

                let idx = struct_info.fields.get_index_of(*attr).unwrap();

                self.vm.add_op1(Op::AccessStructField, idx as u16);
            }
            Expression::CallableCall(_span, callable, args, _type) => {
                // Put all arguments on top of the stack
                for arg in args {
                    self.compile_expression(arg)?;
                }

                // Put the callable on top of the stack
                self.compile_expression(callable)?;

                self.vm.add_op1(Op::CallCallable, args.len() as u16);
            }
            Expression::Boolean(_, val) => {
                let index = self.vm.add_constant(Constant::Boolean(*val));
                self.vm.add_op1(Op::LoadConstant, index);
            }
            Expression::String(_, string_parts) => {
                for part in string_parts {
                    match part {
                        StringPart::Fixed(s) => {
                            let index = self.vm.add_constant(Constant::String(s.clone()));
                            self.vm.add_op1(Op::LoadConstant, index)
                        }
                        StringPart::Interpolation {
                            expr,
                            span: _,
                            format_specifiers,
                        } => {
                            self.compile_expression(expr)?;
                            let index = self.vm.add_constant(Constant::FormatSpecifiers(
                                format_specifiers.map(|s| s.to_compact_string()),
                            ));
                            self.vm.add_op1(Op::LoadConstant, index)
                        }
                    }
                }
                self.vm.add_op1(Op::JoinString, string_parts.len() as u16); // TODO: this can overflow
            }
            Expression::Condition(_, condition, then_expr, else_expr) => {
                self.compile_expression(condition)?;

                let if_jump_offset = self.vm.current_offset() + 1; // +1 for the opcode
                self.vm.add_op1(Op::JumpIfFalse, 0xffff);

                self.compile_expression(then_expr)?;

                let else_jump_offset = self.vm.current_offset() + 1;
                self.vm.add_op1(Op::Jump, 0xffff);

                let else_block_offset = self.vm.current_offset();
                self.vm
                    .patch_u16_value_at(if_jump_offset, else_block_offset - (if_jump_offset + 2));

                self.compile_expression(else_expr)?;

                let end_offset = self.vm.current_offset();

                self.vm
                    .patch_u16_value_at(else_jump_offset, end_offset - (else_jump_offset + 2));
            }
            Expression::List(_, elements, _) => {
                for element in elements {
                    self.compile_expression(element)?;
                }

                self.vm.add_op1(Op::BuildList, elements.len() as u16);
            }
            Expression::TypedHole(_, _) => {
                unreachable!("Typed holes cause type inference errors")
            }
        };

        Ok(())
    }

    fn compile_define_variable(&mut self, define_variable: &DefineVariable) -> Result<()> {
        let DefineVariable(identifier, decorators, expr, _annotation, _type, _readable_type) =
            define_variable;
        let current_depth = self.current_depth();

        // For variables, we ignore the prefix info and only use the names
        let aliases = crate::decorator::name_and_aliases(identifier, decorators)
            .map(|(name, _)| name.to_compact_string())
            .collect::<Vec<_>>();
        let metadata = LocalMetadata {
            name: crate::decorator::name(decorators).map(CompactString::from),
            url: crate::decorator::url(decorators).map(CompactString::from),
            description: crate::decorator::description(decorators),
            aliases: aliases.clone(),
        };

        for alias_name in aliases {
            self.compile_expression(expr)?;

            self.locals[current_depth].push(Local {
                identifier: alias_name.clone(),
                depth: current_depth,
                metadata: metadata.clone(),
            });
        }
        Ok(())
    }

    fn compile_statement(
        &mut self,
        stmt: &Statement,
        dimension_registry: &DimensionRegistry,
    ) -> Result<()> {
        match stmt {
            Statement::Expression(expr) => {
                self.compile_expression(expr)?;
                self.vm.add_op(Op::Return);
            }
            Statement::DefineVariable(define_variable) => {
                self.compile_define_variable(define_variable)?
            }
            Statement::DefineFunction(
                name,
                _decorators,
                _type_parameters,
                parameters,
                Some(expr),
                local_variables,
                _function_type,
                _return_type_annotation,
                _readable_return_type,
            ) => {
                self.vm.begin_function(name);

                self.locals.push(vec![]);

                let current_depth = self.current_depth();
                for parameter in parameters {
                    self.locals[current_depth].push(Local {
                        identifier: parameter.1.to_compact_string(),
                        depth: current_depth,
                        metadata: LocalMetadata::default(),
                    });
                }
                for local_variables in local_variables {
                    self.compile_define_variable(local_variables)?;
                }

                self.compile_expression(expr)?;

                self.vm.add_op(Op::Return);

                self.locals.pop();

                self.vm.end_function();

                self.functions.insert(name.to_compact_string(), false);
            }
            Statement::DefineFunction(
                name,
                _decorators,
                _type_parameters,
                parameters,
                None,
                _local_variables,
                _return_type,
                _return_type_annotation,
                _readable_return_type,
            ) => {
                // Declaring a foreign function does not generate any bytecode. But we register
                // its name and arity here to be able to distinguish it from normal functions.

                self.vm
                    .add_foreign_function(name, parameters.len()..=parameters.len());

                self.functions.insert(name.to_compact_string(), true);
            }
            Statement::DefineDimension(_name, _dexprs) => {
                // Declaring a dimension is like introducing a new type. The information
                // is only relevant for the type checker. Nothing happens at run time.
            }
            Statement::DefineBaseUnit(unit_name, decorators, annotation, type_) => {
                let aliases = decorator::name_and_aliases(unit_name, decorators)
                    .map(|(name, ap)| (name.to_compact_string(), ap))
                    .collect();

                self.vm
                    .unit_registry
                    .add_base_unit(
                        unit_name,
                        UnitMetadata {
                            type_: type_.to_concrete_type(), // Base unit types can never be generic
                            readable_type: annotation
                                .as_ref()
                                .map(|a| a.pretty_print())
                                .unwrap_or(type_.to_readable_type(dimension_registry, false)),
                            aliases,
                            name: decorator::name(decorators).map(CompactString::from),
                            canonical_name: decorator::get_canonical_unit_name(
                                unit_name, decorators,
                            ),
                            url: decorator::url(decorators).map(CompactString::from),
                            description: decorator::description(decorators),
                            binary_prefixes: decorators.contains(&Decorator::BinaryPrefixes),
                            metric_prefixes: decorators.contains(&Decorator::MetricPrefixes),
                        },
                    )
                    .map_err(RuntimeError::UnitRegistryError)?;

                let constant_idx = self.vm.add_constant(Constant::Unit(Unit::new_base(
                    unit_name.to_compact_string(),
                    crate::decorator::get_canonical_unit_name(unit_name, &decorators[..]),
                )));
                for (name, _) in decorator::name_and_aliases(unit_name, decorators) {
                    self.unit_name_to_constant_index
                        .insert(name.into(), constant_idx);
                }
            }
            Statement::DefineDerivedUnit(
                unit_name,
                expr,
                decorators,
                annotation,
                type_,
                _readable_type,
            ) => {
                let aliases = decorator::name_and_aliases(unit_name, decorators)
                    .map(|(name, ap)| (name.to_compact_string(), ap))
                    .collect();

                let constant_idx = self.vm.add_constant(Constant::Unit(Unit::new_base(
                    CompactString::const_new("<dummy>"),
                    CanonicalName {
                        name: CompactString::const_new("<dummy>"),
                        accepts_prefix: AcceptsPrefix::both(),
                    },
                ))); // TODO: dummy is just a temp. value until the SetUnitConstant op runs
                let unit_information_idx = self.vm.add_unit_information(
                    unit_name,
                    Some(
                        &crate::decorator::get_canonical_unit_name(unit_name, &decorators[..]).name,
                    ),
                    UnitMetadata {
                        type_: type_.to_concrete_type(), // We guarantee that derived-unit definitions do not contain generics, so no TGen(..)s can escape
                        readable_type: annotation
                            .as_ref()
                            .map(|a| a.pretty_print())
                            .unwrap_or(type_.to_readable_type(dimension_registry, false)),
                        aliases,
                        name: decorator::name(decorators).map(CompactString::from),
                        canonical_name: decorator::get_canonical_unit_name(unit_name, decorators),
                        url: decorator::url(decorators).map(CompactString::from),
                        description: decorator::description(decorators),
                        binary_prefixes: decorators.contains(&Decorator::BinaryPrefixes),
                        metric_prefixes: decorators.contains(&Decorator::MetricPrefixes),
                    },
                ); // TODO: there is some asymmetry here because we do not introduce identifiers for base units

                self.compile_expression(expr)?;
                self.vm
                    .add_op2(Op::SetUnitConstant, unit_information_idx, constant_idx);

                // TODO: code duplication with DeclareBaseUnit branch above
                for (name, _) in decorator::name_and_aliases(unit_name, decorators) {
                    self.unit_name_to_constant_index
                        .insert(name.into(), constant_idx);
                }
            }
            Statement::ProcedureCall(ProcedureKind::Type, args) => {
                assert_eq!(args.len(), 1);
                let arg = &args[0];

                use crate::markup as m;
                let idx = self.vm.add_string(
                    m::dimmed("=") + m::whitespace(" ") + arg.get_type_scheme().pretty_print(), // TODO
                );
                self.vm.add_op1(Op::PrintString, idx);
            }
            Statement::ProcedureCall(kind, args) => {
                // Put all arguments on top of the stack
                for arg in args {
                    self.compile_expression(arg)?;
                }

                let name = &ffi::procedures().get(kind).unwrap().name;

                let callable_idx = self.vm.get_ffi_callable_idx(name).unwrap();

                let arg_spans = args.iter().map(|a| a.full_span()).collect();
                let spans_idx = self.vm.add_procedure_arg_span(arg_spans);

                self.vm.add_op3(
                    Op::FFICallProcedure,
                    callable_idx,
                    args.len() as u16,
                    spans_idx,
                );
                // TODO: check overflow
            }
            Statement::DefineStruct(struct_info) => {
                self.vm.add_struct_info(struct_info);
            }
        }

        Ok(())
    }

    fn run(&mut self, settings: &mut InterpreterSettings) -> Result<InterpreterResult> {
        let mut ctx = ExecutionContext {
            print_fn: &mut settings.print_fn,
        };

        self.vm.disassemble();

        let result = self.vm.run(&mut ctx);

        let result = match result {
            Ok(InterpreterResult::Value(Value::Quantity(q))) => {
                Ok(InterpreterResult::Value(Value::Quantity(q.full_simplify())))
            }
            r => r,
        };

        self.vm.debug();

        result
    }

    pub(crate) fn set_debug(&mut self, activate: bool) {
        self.vm.set_debug(activate);
    }

    fn current_depth(&self) -> usize {
        self.locals.len() - 1
    }

    pub fn get_defining_unit(&self, unit_name: &str) -> Option<&Unit> {
        self.unit_name_to_constant_index
            .get(unit_name)
            .and_then(|idx| self.vm.constants.get(*idx as usize))
            .and_then(|constant| match constant {
                Constant::Unit(u) => Some(u),
                _ => None,
            })
    }

    pub fn lookup_global(&self, name: &str) -> Option<&Local> {
        self.locals[0].iter().find(|l| l.identifier == name)
    }
}

impl Interpreter for BytecodeInterpreter {
    fn new() -> Self {
        Self {
            vm: Vm::new(),
            locals: vec![vec![]],
            unit_name_to_constant_index: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn interpret_statements(
        &mut self,
        settings: &mut InterpreterSettings,
        statements: &[Statement],
        dimension_registry: &DimensionRegistry,
    ) -> Result<InterpreterResult> {
        for statement in statements {
            self.compile_statement(statement, dimension_registry)?;
        }

        self.run(settings)
    }

    fn get_unit_registry(&self) -> &UnitRegistry {
        &self.vm.unit_registry
    }
}
