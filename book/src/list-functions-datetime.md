# Date and time

See [this page](./date-and-time.md) for a general introduction to date and time handling in Numbat.

Defined in: `datetime::functions`, `datetime::human`

### `now`
Returns the current date and time.

```nbt
fn now() -> DateTime
```

### `datetime`
Parses a string (date and time) into a `DateTime` object. See [here](./date-and-time.md#date-time-formats) for an overview of the supported formats.

```nbt
fn datetime(input: String) -> DateTime
```

### `format_datetime`
Formats a `DateTime` object as a string.

```nbt
fn format_datetime(format: String, input: DateTime) -> String
```

### `get_local_timezone`
Returns the users local timezone.

```nbt
fn get_local_timezone() -> String
```

### `tz`
Returns a timezone conversion function, typically used with the conversion operator.

```nbt
fn tz(tz: String) -> Fn[(DateTime) -> DateTime]
```

### `unixtime`
Converts a `DateTime` to a UNIX timestamp. Can be used on the right hand side of a conversion operator: `now() -> unixtime`.

```nbt
fn unixtime(input: DateTime) -> Scalar
```

### `from_unixtime`
Converts a UNIX timestamp to a `DateTime` object.

```nbt
fn from_unixtime(input: Scalar) -> DateTime
```

### `today`
Returns the current date at midnight (in the local time).

```nbt
fn today() -> DateTime
```

### `date`
Parses a string (only date) into a `DateTime` object.

```nbt
fn date(input: String) -> DateTime
```

### `time`
Parses a string (time only) into a `DateTime` object.

```nbt
fn time(input: String) -> DateTime
```

### `julian_date` (Julian date)
Convert a `DateTime` to a Julian date, the number of days since the origin of the Julian date system (noon on November 24, 4714 BC in the propleptic Gregorian calendar).
More information [here](https://en.wikipedia.org/wiki/Julian_day).

```nbt
fn julian_date(dt: DateTime) -> Time
```

### `human` (Human-readable time duration)
Converts a time duration to a human-readable string in days, hours, minutes and seconds.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn human(time: Time) -> String
```

