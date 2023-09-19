use std::path::PathBuf;

use numbat::resolver::{ModuleImporter, ModulePath};

#[derive(Debug, Clone, Default)]
pub struct WasmImporter {}

impl ModuleImporter for WasmImporter {
    fn import(&self, path: &ModulePath) -> Option<(String, Option<PathBuf>)> {
        let path = path.to_string();
        let code = match path.as_str() {
            "core::dimensions" => include_str!("../../modules/core/dimensions.nbt"),
            "core::quantity" => include_str!("../../modules/core/quantity.nbt"),
            "core::scalar" => include_str!("../../modules/core/scalar.nbt"),
            "math::constants" => include_str!("../../modules/math/constants.nbt"),
            "math::functions" => include_str!("../../modules/math/functions.nbt"),
            "math::trigonometry_extra" => include_str!("../../modules/math/trigonometry_extra.nbt"),
            "physics::constants" => include_str!("../../modules/physics/constants.nbt"),
            "physics::temperature_conversion" => {
                include_str!("../../modules/physics/temperature_conversion.nbt")
            }
            "prelude" => include_str!("../../modules/prelude.nbt"),
            "units::astronomical" => include_str!("../../modules/units/astronomical.nbt"),
            "units::bit" => include_str!("../../modules/units/bit.nbt"),
            "units::cgs" => include_str!("../../modules/units/cgs.nbt"),
            "units::currencies" => include_str!("../../modules/units/currencies.nbt"),
            "units::currency" => include_str!("../../modules/units/currency.nbt"),
            "units::fff" => include_str!("../../modules/units/fff.nbt"),
            "units::hartree" => include_str!("../../modules/units/hartree.nbt"),
            "units::imperial" => include_str!("../../modules/units/imperial.nbt"),
            "units::misc" => include_str!("../../modules/units/misc.nbt"),
            "units::nautical" => include_str!("../../modules/units/nautical.nbt"),
            "units::partsperx" => include_str!("../../modules/units/partsperx.nbt"),
            "units::placeholder" => include_str!("../../modules/units/placeholder.nbt"),
            "units::si" => include_str!("../../modules/units/si.nbt"),
            "units::stoney" => include_str!("../../modules/units/stoney.nbt"),
            "units::time" => include_str!("../../modules/units/time.nbt"),
            "units::us_customary" => include_str!("../../modules/units/us_customary.nbt"),

            _ => return None,
        };

        Some((code.to_string(), None))
    }
}
