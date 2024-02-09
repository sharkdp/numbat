use chrono::{DateTime, FixedOffset, Offset, TimeZone};
use chrono_tz::Tz;

fn get_local_timezone() -> Option<Tz> {
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
pub fn local_offset_for_datetime<O: TimeZone + Offset>(dt: &DateTime<O>) -> FixedOffset {
    get_local_timezone()
        .map(|tz| dt.with_timezone(&tz).offset().fix())
        .unwrap_or_else(|| dt.offset().fix())
}
