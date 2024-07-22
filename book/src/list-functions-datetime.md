<!-- NOTE! This file is auto-generated -->

# Date and time

### `now`
Returns the current date and time.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn now() -> DateTime
```
(defined in *datetime::functions*)

### `datetime`
Parses a string (date and time) into a DateTime object. See https://numbat.dev/doc/date-and-time.html#date-time-formats for an overview of the supported formats.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn datetime(input: String) -> DateTime
```
(defined in *datetime::functions*)

### `format_datetime`
Formats a DateTime object as a string.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn format_datetime(format: String, input: DateTime) -> String
```
(defined in *datetime::functions*)

### `get_local_timezone`
Returns the users local timezone.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn get_local_timezone() -> String
```
(defined in *datetime::functions*)

### `tz`
Returns a timezone conversion function, typically used with the conversion operator.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn tz(tz: String) -> Fn[(DateTime) -> DateTime]
```
(defined in *datetime::functions*)

### `unixtime`
Converts a DateTime to a UNIX timestamp.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn unixtime(input: DateTime) -> Scalar
```
(defined in *datetime::functions*)

### `from_unixtime`
Converts a UNIX timestamp to a DateTime object.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn from_unixtime(input: Scalar) -> DateTime
```
(defined in *datetime::functions*)

### `today`
Returns the current date at midnight (in the local time).
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn today() -> DateTime
```
(defined in *datetime::functions*)

### `date`
Parses a string (only date) into a DateTime object.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn date(input: String) -> DateTime
```
(defined in *datetime::functions*)

### `time`
Parses a string (only time) into a DateTime object.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn time(input: String) -> DateTime
```
(defined in *datetime::functions*)

### `human` (Human-readable time duration)
Converts a time duration to a human-readable string in days, hours, minutes and seconds.
More information [here](https://numbat.dev/doc/date-and-time.html).

```nbt
fn human(time: Time) -> String
```
(defined in *datetime::human*)

