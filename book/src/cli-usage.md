# Usage

## Modes

You can run the Numbat command-line application in three different modes:

| Mode | Command to run |
|---|---|
| Start an interactive session (REPL) | `numbat` |
| Run a Numbat program | `numbat script.nbt` |
| Evaluate a single expression | `numbat -e '30 km/h -> mi/h'` |

## Command-line options

See `numbat --help` for more information.

## Interactive sessions

Interactive sessions allow you to perform a sequence of calculations. You can use the special identifiers
`ans` or `_` to refer to the result of the last calculation. For example:

``` numbat
>>> 60 kW h / 150 kW

    = 0.4 h

>>> ans -> minutes

    = 24 min
```

### Commands

There is a set of special commands that only work in interactive mode:

| Command | Action |
|---------|--------|
| `list` | List all functions, dimensions, variables and units |
| `list <what>` | Where `<what>` can be `functions`, `dimensions`, `variables`, `units` |
| `info <identifier>` | Get more information about units, variables, and functions |
| `clear` | Clear screen |
| `help`, `?` | View short help text |
| `save` | Save the current session history to file `history.nbt` in the current directory |
| `save <dst>` | Save the current session history to file `<dst>` relative to the current working directory |
| `quit`, `exit` | Quit the session |

### Key bindings

In interactive command-line mode, you can use the following key bindings. Most importantly,
`Tab` for auto-completion, arrow keys and `Ctrl-R` for browsing the command history, and
`Ctrl-D` for exiting the interactive session.

| Key sequence | Action |
|--------------|--------|
| `Tab`, `Ctrl`-`I` | Auto-completion |
| `Ctrl`-`D` | Quit |
| `Ctrl`-`L` | Clear screen |
| `Up`, `Down` | Browse command history |
| `Ctrl`-`R` | Search command history |
| `Ctrl`-`C` | Clear the current line |
| `Alt`-`Enter` | Insert newline |
| `Home`, `Ctrl`-`A` | Move cursor to the beginning of the line |
| `End`, `Ctrl`-`E` | Move cursor to the end of the line |
| `Ctrl`-`W` | Delete word leading up to cursor |
