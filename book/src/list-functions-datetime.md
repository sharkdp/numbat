# Date and time

## Basics

Defined in: `datetime::functions`

### `now`
Returns the current date and time.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn now() -> DateTime
```

### `datetime`
Parses a string (date and time) into a DateTime object. See [this page](/doc/date-and-time.html#date-time-formats) for an overview of the supported formats.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn datetime(input: String) -> DateTime
```

### `format_datetime`
Formats a DateTime object as a string.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn format_datetime(format: String, input: DateTime) -> String
```

### `get_local_timezone`
Returns the users local timezone.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn get_local_timezone() -> String
```

### `tz`
Returns a timezone conversion function, typically used with the conversion operator.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn tz(tz: String) -> Fn[(DateTime) -> DateTime]
```

### `unixtime`
Converts a DateTime to a UNIX timestamp.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn unixtime(input: DateTime) -> Scalar
```

### `from_unixtime`
Converts a UNIX timestamp to a DateTime object.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn from_unixtime(input: Scalar) -> DateTime
```

### `today`
Returns the current date at midnight (in the local time).
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn today() -> DateTime
```

### `date`
Parses a string (only date) into a DateTime object.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn date(input: String) -> DateTime
```

### `time`
Parses a string (only time) into a DateTime object.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn time(input: String) -> DateTime
```

## Human-readable

Defined in: `datetime::human`

### `human` (Human-readable time duration)
Converts a time duration to a human-readable string in days, hours, minutes and seconds.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn human(time: Time) -> String
```
