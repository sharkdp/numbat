# Date and time

Numbat supports date and time handling based on the [proleptic Gregorian calendar](https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar),
which is the (usual) Gregorian calendar extended to dates before its introduction in 1582. Julian calendar dates are currently not supported.

A few examples of useful operations that can be performed on dates and times:

```nbt
# Which date is 40 days from now?
now() + 40 days

# Which date was 1 million seconds ago?
now() - 1 million seconds

# How many days are left until September 1st?
parse_datetime("2024-11-01 12:30:00") - now() -> days

# What time is it in Nepal right now?
now() -> "Asia/Kathmandu"

# What is the local time when it is 2024-11-01 12:30:00 in Australia?
parse_datetime("2024-11-01 12:30:00 Australia/Sydney") -> "local"

# What is the current UNIX timestamp?
now() // to_unixtime

# What is the date corresponding to the UNIX timestamp 1707568901?
from_unixtime(1707568901)
```

As you can see from the examples, datetimes are either created using the `now()` function or by
parsing a string using the `parse_datetime` function. The following formats are supported.
UTC offsets are mandatory for the RFC 3339 and RFC 2822 formats. The other formats can optionally
include a time zone name or UTC offset. If no time zone is specified, the local time zone is used.

| Format | Examples |
| ------ | ------- |
| [RFC 3339](https://tools.ietf.org/html/rfc3339) | `2024-02-10T12:30:00Z`<br>`2024-02-10T06:30:00-06:00` |
| [RFC 2822](https://tools.ietf.org/html/rfc2822) | `Sat, 10 Feb 2024 12:30:00 Z`<br>`Sat, 10 Feb 2024 06:30:00 -0600` |
| `%Y-%m-%d %H:%M:%S%.f` | `2024-02-10 12:30:00`<br>`2024-02-10 06:30:00 -0600`<br>`2024-02-10 07:30:00 US/Eastern`<br>`2024-02-10 12:30:00.123456` |
| `%Y/%m/%d %H:%M:%S%.f` | same, but with `/` separator |
| `%Y-%m-%d %H:%M` | `2024-02-10 12:30`<br>`2024-02-10 06:30 -0600`<br>`2024-02-10 07:30 US/Eastern` |
| `%Y/%m/%d %H:%M` | same, but with `/` separator |
| `%Y-%m-%d %I:%M:%S%.f %p` | `2024-02-10 12:30:00 PM`<br>`2024-02-10 06:30:00 AM -0600`<br>`2024-02-10 07:30:00 AM US/Eastern`<br>`2024-02-10 12:30:00.123456 PM` |
| `%Y/%m/%d %I:%M:%S%.f %p` | same, but with `/` separator |
| `%Y-%m-%d %I:%M %p` | `2024-02-10 12:30 PM`<br>`2024-02-10 06:30 AM -0600`<br>`2024-02-10 07:30 AM US/Eastern` |
| `%Y/%m/%d %I:%M %p` | same, but with `/` separator |


## Date and time arithmetic

The following operations are supported for `DateTime` objects:

| Left | Operator | Right | Result |
| ---- | -------- | ----- | ------ |
| `DateTime` | `-` | `DateTime` | Duration between the two dates as a `Time` |
| `DateTime` | `+` | `Time` | New `DateTime` by adding the duration to the date |
| `DateTime` | `-` | `Time` | New `DateTime` by subtracting the duration from the date |
| `DateTime` | `->` | `String` | Converts the datetime to the specified time zone |

## Date and time functions

The following functions are available for date and time handling:

- `now() -> DateTime`: Returns the current date and time.
- `parse_datetime(input: String) -> DateTime`: Parses a string into a `DateTime` object.
- `format_datetime(format: String, dt: DateTime) -> String`: Formats a `DateTime` object as a string.
- `to_unixtime(dt: DateTime) -> Scalar`: Converts a `DateTime` to a UNIX timestamp.
- `from_unixtime(ut: Scalar) -> DateTime`: Converts a UNIX timestamp to a `DateTime` object.
