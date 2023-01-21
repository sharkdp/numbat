use std::collections::HashMap;

use once_cell::sync::OnceCell;

use crate::{ast::MacroKind, number::Number, quantity::Quantity};

#[derive(Clone)]
pub(crate) enum Callable {
    Function(fn(&[Quantity]) -> Quantity),
    Macro(fn(&[Quantity]) -> ()),
}

#[derive(Clone)]
pub(crate) struct ForeignFunction {
    pub(crate) name: String,
    pub(crate) arity: usize,
    pub(crate) callable: Callable,
}

static FFI_MACROS: OnceCell<HashMap<MacroKind, ForeignFunction>> = OnceCell::new();
static FFI_FUNCTIONS: OnceCell<HashMap<&'static str, ForeignFunction>> = OnceCell::new();

pub(crate) fn macros() -> &'static HashMap<MacroKind, ForeignFunction> {
    FFI_MACROS.get_or_init(|| {
        let mut m = HashMap::new();

        m.insert(
            MacroKind::Print,
            ForeignFunction {
                name: "print".into(),
                arity: 1,
                callable: Callable::Macro(print),
            },
        );
        m.insert(
            MacroKind::AssertEq,
            ForeignFunction {
                name: "assert_eq".into(),
                arity: 2,
                callable: Callable::Macro(assert_eq),
            },
        );

        m
    })
}

pub(crate) fn functions() -> &'static HashMap<&'static str, ForeignFunction> {
    FFI_FUNCTIONS.get_or_init(|| {
        let mut m = HashMap::new();

        m.insert(
            "abs",
            ForeignFunction {
                name: "abs".into(),
                arity: 1,
                callable: Callable::Function(abs),
            },
        );
        m.insert(
            "sin",
            ForeignFunction {
                name: "sin".into(),
                arity: 1,
                callable: Callable::Function(sin),
            },
        );
        m.insert(
            "atan2",
            ForeignFunction {
                name: "atan2".into(),
                arity: 2,
                callable: Callable::Function(atan2),
            },
        );

        m
    })
}

fn print(args: &[Quantity]) {
    assert!(args.len() == 1);

    println!("{}", args[0]);
}

fn assert_eq(args: &[Quantity]) {
    assert!(args.len() == 2);

    // TODO
}

fn abs(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let value = args[0].unsafe_value().to_f64();
    Quantity::new(Number::from_f64(value.abs()), args[0].unit().clone())
}

fn sin(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 1);

    let input = args[0].as_scalar().unwrap().to_f64();
    Quantity::from_scalar(input.sin())
}

fn atan2(args: &[Quantity]) -> Quantity {
    assert!(args.len() == 2);

    let input0 = args[0].unsafe_value().to_f64();
    let input1 = args[1].unsafe_value().to_f64();
    Quantity::from_scalar(input0.atan2(input1))
}
