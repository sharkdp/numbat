use jiff::{civil::DateTime, fmt::rfc2822, tz::TimeZone, Zoned};

pub fn get_local_timezone_or_utc() -> TimeZone {
    TimeZone::system()
}

pub fn parse_datetime(input: &str) -> Option<Zoned> {
    if let Ok(dt) = DateTime::strptime("%Y-%m-%dT%H:%M:%SZ", input) {
        // RFC 3339
        Some(dt.to_zoned(TimeZone::UTC).unwrap()) // TODO
    } else if let Ok(dt) = rfc2822::parse(input) {
        Some(dt)
    } else {
        const FORMATS: [&str; 8] = [
            // 24 hour formats:
            "%Y-%m-%d %H:%M:%S", // TODO: add support for fractional seconds
            "%Y/%m/%d %H:%M:%S", // TODO: add support for fractional seconds
            "%Y-%m-%d %H:%M",
            "%Y/%m/%d %H:%M",
            // 12 hour formats:
            "%Y-%m-%d %I:%M:%S %p", // TODO: add support for fractional seconds
            "%Y-%m-%d %I:%M %p",
            "%Y/%m/%d %I:%M:%S %p", // TODO: add support for fractional seconds
            "%Y/%m/%d %I:%M %p",
        ];

        for format in FORMATS {
            // Try to match the given format plus an additional UTC offset (%z)
            if let Ok(dt) = Zoned::strptime(&format!("{format} %z"), input) {
                return Some(dt);
            }

            // Try to match the given format plus an additional timezone name (%Z).
            // jiff does not support %Z, so we implement this ourselves. We were
            // warned by developers before us not to write timezone-related code on
            // our own, so we're probably going to regret this.

            // Get the last space-separated word in the input string, and try to parse it
            // as a timezone specifier, then try to match the rest of the string with the
            // given format.
            if let Some((rest, potential_timezone_name)) = input.rsplit_once(' ') {
                if let Ok(tz) = TimeZone::get(potential_timezone_name) {
                    if let Ok(datetime) = DateTime::strptime(format, rest) {
                        return Some(datetime.to_zoned(tz).unwrap()); // TODO
                    }
                }
            }

            // Without timezone/offset
            if let Ok(dt) = DateTime::strptime(format, input) {
                return Some(dt.to_zoned(get_local_timezone_or_utc()).unwrap()); // TODO
            }
        }

        None
    }
}

pub fn to_string(dt: &Zoned) -> String {
    if dt.time_zone() == &TimeZone::UTC {
        dt.strftime("%Y-%m-%d %H:%M:%S UTC").to_string()
    } else {
        dt.strftime("%Y-%m-%d %H:%M:%S %Z (UTC %z)").to_string()
    }
}
