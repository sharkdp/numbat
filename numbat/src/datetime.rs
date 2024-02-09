use chrono::{DateTime, FixedOffset, LocalResult, Offset, ParseError, TimeZone};
use chrono_tz::Tz;

pub fn get_local_timezone() -> Option<Tz> {
    let tz_str = iana_time_zone::get_timezone().ok()?;
    tz_str.parse().ok()
}

/// We use this function to get the UTC offset corresponding to
/// the local timezone *at the specific instant in time* specified
/// by 'dt', which might be different from the *current* UTC offset.
///
/// For example, in the timezone Europe/Berlin in 2024, we expect a
/// UTC offset of +01:00 for 'dt's in winter, while we expect a UTC
/// offset of +02:00 for 'dt's in summer, due to DST. If we were to
/// use chronos 'Local', we would get a UTC offset depending on when
/// we run the program.
pub fn local_offset_for_datetime<O: TimeZone>(dt: &DateTime<O>) -> FixedOffset {
    get_local_timezone()
        .map(|tz| dt.with_timezone(&tz).offset().fix())
        .unwrap_or_else(|| dt.offset().fix())
}

pub fn parse_datetime(input: &str) -> Result<Option<DateTime<FixedOffset>>, ParseError> {
    if let Ok(dt) = chrono::DateTime::parse_from_rfc3339(input) {
        Ok(Some(dt))
    } else if let Ok(dt) = chrono::DateTime::parse_from_rfc2822(input) {
        Ok(Some(dt))
    } else {
        const FORMATS: [&str; 8] = [
            // 24 hour formats:
            "%Y-%m-%d %H:%M:%S%.f",
            "%Y/%m/%d %H:%M:%S%.f",
            "%Y-%m-%d %H:%M",
            "%Y/%m/%d %H:%M",
            // 12 hour formats:
            "%Y-%m-%d %I:%M:%S%.f %p",
            "%Y-%m-%d %I:%M %p",
            "%Y/%m/%d %I:%M:%S%.f %p",
            "%Y/%m/%d %I:%M %p",
        ];

        for format in FORMATS {
            // With UTC offset
            if let Ok(dt) = chrono::DateTime::parse_from_str(input, &format!("{format} %z")) {
                return Ok(Some(dt));
            }

            // Without offset
            if let Ok(ndt) = chrono::NaiveDateTime::parse_from_str(input, format) {
                if let LocalResult::Single(dt) =
                    ndt.and_local_timezone(get_local_timezone().unwrap_or(chrono_tz::UTC))
                {
                    return Ok(Some(dt.with_timezone(&local_offset_for_datetime(&dt))));
                }
            }
        }

        Ok(None)
    }
}
