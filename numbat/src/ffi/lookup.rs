use compact_str::CompactString;

use super::macros::*;
use super::Args;
use super::Result;
use crate::quantity::Quantity;
use crate::typed_ast::DType;
use crate::value::Value;
use crate::RuntimeError;

pub fn _get_chemical_element_data_raw(mut args: Args) -> Result<Value> {
    use crate::span::{ByteIndex, Span};
    use crate::typed_ast::StructInfo;
    use crate::typed_ast::Type;
    use indexmap::IndexMap;
    use mendeleev::{Electronvolt, GramPerCubicCentimeter, Kelvin, KiloJoulePerMole};
    use std::sync::Arc;

    let pattern = string_arg!(args).to_lowercase();

    if let Some(element) = mendeleev::Element::list()
        .iter()
        .find(|e| e.name().to_lowercase() == pattern || e.symbol().to_lowercase() == pattern)
    {
        let unknown_span = Span {
            start: ByteIndex(0),
            end: ByteIndex(0),
            code_source_id: 0,
        };

        let type_scalar = Type::Dimension(DType::scalar());

        let mut fields: IndexMap<CompactString, (Span, Type)> = IndexMap::new();
        fields.insert(
            CompactString::const_new("symbol"),
            (unknown_span, Type::String),
        );
        fields.insert(
            CompactString::const_new("name"),
            (unknown_span, Type::String),
        );
        fields.insert(
            CompactString::const_new("atomic_number"),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            CompactString::const_new("group"),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            CompactString::const_new("group_name"),
            (unknown_span, Type::String),
        );
        fields.insert(
            CompactString::const_new("period"),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            CompactString::const_new("melting_point_kelvin"),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            CompactString::const_new("boiling_point_kelvin"),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            CompactString::const_new("density_gram_per_cm3"),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            CompactString::const_new("electron_affinity_electronvolt"),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            CompactString::const_new("ionization_energy_electronvolt"),
            (unknown_span, type_scalar.clone()),
        );
        fields.insert(
            CompactString::const_new("vaporization_heat_kilojoule_per_mole"),
            (unknown_span, type_scalar.clone()),
        );

        let info = StructInfo {
            name: CompactString::const_new("_ChemicalElementRaw"),
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
        Err(RuntimeError::ChemicalElementNotFound(pattern.to_string()))
    }
}
