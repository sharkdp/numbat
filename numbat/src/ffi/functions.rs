use std::collections::HashMap;
use std::sync::OnceLock;

use super::macros::*;
use crate::{
    currency::ExchangeRatesCache, quantity::Quantity, typed_ast::DType, value::Value, RuntimeError,
};

use super::{Callable, ForeignFunction, Result};

static FFI_FUNCTIONS: OnceLock<HashMap<String, ForeignFunction>> = OnceLock::new();

pub(crate) fn functions() -> &'static HashMap<String, ForeignFunction> {
    use super::datetime::*;
    use super::lists::*;
    use super::math::*;
    use super::strings::*;

    FFI_FUNCTIONS.get_or_init(|| {
        let mut m = HashMap::new();

        macro_rules! insert_function {
            ($fn_name:expr, $callable:expr, $arity:expr) => {
                m.insert(
                    $fn_name.to_string(),
                    ForeignFunction {
                        name: $fn_name.to_string(),
                        arity: $arity,
                        callable: Callable::Function(Box::new($callable)),
                    },
                );
            };
            ($callable:expr, $arity:expr) => {
                insert_function!(stringify!($callable), $callable, $arity);
            };
        }

        insert_function!(error, 1..=1);
        insert_function!(unit_of, 1..=1);
        insert_function!(random, 0..=0);
        insert_function!("mod", mod_, 2..=2);

        // Math
        insert_function!(abs, 1..=1);
        insert_function!(round, 1..=1);
        insert_function!(floor, 1..=1);
        insert_function!(ceil, 1..=1);

        insert_function!(sin, 1..=1);
        insert_function!(cos, 1..=1);
        insert_function!(tan, 1..=1);
        insert_function!(asin, 1..=1);
        insert_function!(acos, 1..=1);
        insert_function!(atan, 1..=1);
        insert_function!(atan2, 2..=2);
        insert_function!(sinh, 1..=1);
        insert_function!(cosh, 1..=1);
        insert_function!(tanh, 1..=1);
        insert_function!(asinh, 1..=1);
        insert_function!(acosh, 1..=1);
        insert_function!(atanh, 1..=1);
        insert_function!(exp, 1..=1);
        insert_function!(ln, 1..=1);
        insert_function!(log10, 1..=1);
        insert_function!(log2, 1..=1);
        insert_function!(gamma, 1..=1);

        insert_function!(is_nan, 1..=1);
        insert_function!(is_infinite, 1..=1);

        // Lists
        insert_function!(len, 1..=1);
        insert_function!(head, 1..=1);
        insert_function!(tail, 1..=1);
        insert_function!(cons, 2..=2);

        // Strings
        insert_function!(str_length, 1..=1);
        insert_function!(lowercase, 1..=1);
        insert_function!(uppercase, 1..=1);
        insert_function!(str_slice, 3..=3);
        insert_function!(chr, 1..=1);

        // DateTime
        insert_function!(now, 0..=0);
        insert_function!(datetime, 1..=1);
        insert_function!(format_datetime, 2..=2);
        insert_function!(get_local_timezone, 0..=0);
        insert_function!(tz, 1..=1);
        insert_function!(unixtime, 1..=1);
        insert_function!(from_unixtime, 1..=1);

        // Currency
        insert_function!(exchange_rate, 1..=1);

        // Database lookup
        insert_function!(_get_chemical_element_data_raw, 1..=1);

        m
    })
}

fn error(args: &[Value]) -> Result<Value> {
    Err(RuntimeError::UserError(args[0].unsafe_as_string().into()))
}

fn unit_of(args: &[Value]) -> Result<Value> {
    let input_unit = quantity_arg!(args, 0).unit().clone();
    return_quantity!(1.0, input_unit)
}

fn random(_args: &[Value]) -> Result<Value> {
    return_scalar!(rand::random::<f64>())
}

fn exchange_rate(args: &[Value]) -> Result<Value> {
    let rate = args[0].unsafe_as_string();

    let exchange_rates = ExchangeRatesCache::new();

    return_scalar!(exchange_rates.get_rate(rate).unwrap_or(f64::NAN))
}

fn _get_chemical_element_data_raw(args: &[Value]) -> Result<Value> {
    use crate::span::{SourceCodePositition, Span};
    use crate::typed_ast::StructInfo;
    use crate::typed_ast::Type;
    use indexmap::IndexMap;
    use mendeleev::{Electronvolt, GramPerCubicCentimeter, Kelvin, KiloJoulePerMole};
    use std::sync::Arc;

    let pattern = args[0].unsafe_as_string().to_lowercase();

    if let Some(element) = mendeleev::Element::list()
        .iter()
        .find(|e| e.name().to_lowercase() == pattern || e.symbol().to_lowercase() == pattern)
    {
        let unknown_span = Span {
            start: SourceCodePositition::start(),
            end: SourceCodePositition::start(),
            code_source_id: 0,
        };

        let type_scalar = Type::Dimension(DType::scalar());

        let mut fields: IndexMap<String, (Span, Type)> = IndexMap::new();
        fields.insert("symbol".to_string(), (unknown_span, Type::String));
        fields.insert("name".to_string(), (unknown_span, Type::String));
        fields.insert(
            "atomic_number".to_string(),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert("group".to_string(), (unknown_span, type_scalar.clone()));
        fields.insert("group_name".to_string(), (unknown_span, Type::String));
        fields.insert("period".to_string(), (unknown_span, type_scalar.clone()));
        fields.insert(
            "melting_point_kelvin".to_string(),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            "boiling_point_kelvin".to_string(),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            "density_gram_per_cm3".to_string(),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            "electron_affinity_electronvolt".to_string(),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            "ionization_energy_electronvolt".to_string(),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            "vaporization_heat_kilojoule_per_mole".to_string(),
            (unknown_span, type_scalar.clone()),
        );

        let info = StructInfo {
            name: "_ChemicalElementRaw".to_string(),
            definition_span: unknown_span,
            fields,
        };
        Ok(Value::StructInstance(
            Arc::new(info),
            vec![
                Value::String(element.symbol().into()),
                Value::String(element.name().into()),
                Value::Quantity(Quantity::from_scalar(element.atomic_number() as f64)),
                Value::Quantity(Quantity::from_scalar(
                    element
                        .group()
                        .map_or(f64::NAN, |g| g.group_number() as f64),
                )),
                Value::String(
                    element
                        .group()
                        .map(|g| g.group_name().unwrap_or("unknown").into())
                        .unwrap_or("unknown".into()),
                ),
                Value::Quantity(Quantity::from_scalar(element.period() as f64)),
                Value::Quantity(Quantity::from_scalar(
                    element
                        .melting_point()
                        .map(|Kelvin(k)| k)
                        .unwrap_or(f64::NAN),
                )),
                Value::Quantity(Quantity::from_scalar(
                    element
                        .boiling_point()
                        .map(|Kelvin(k)| k)
                        .unwrap_or(f64::NAN),
                )),
                Value::Quantity(Quantity::from_scalar(
                    element
                        .density()
                        .map(|GramPerCubicCentimeter(d)| d)
                        .unwrap_or(f64::NAN),
                )),
                Value::Quantity(Quantity::from_scalar(
                    element
                        .electron_affinity()
                        .map(|Electronvolt(e)| e)
                        .unwrap_or(f64::NAN),
                )),
                Value::Quantity(Quantity::from_scalar(
                    element
                        .ionization_energy()
                        .map(|Electronvolt(e)| e)
                        .unwrap_or(f64::NAN),
                )),
                Value::Quantity(Quantity::from_scalar(
                    element
                        .evaporation_heat()
                        .map(|KiloJoulePerMole(e)| e)
                        .unwrap_or(f64::NAN),
                )),
            ],
        ))
    } else {
        Err(RuntimeError::ChemicalElementNotFound(pattern))
    }
}
