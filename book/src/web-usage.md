# Usage

The browser-based version of Numbat is online at <https://numbat.dev/>.

## Interactive terminal

The terminal allows you to perform a sequence of calculations.
You can use the arrow keys to browse through the command history.
The special identifiers `ans` and `_` refer to the result of the last calculation. For example:

``` numbat
>>> 60 kW h / 150 kW

    = 0.4 h

>>> ans -> minutes

    = 24 min
```

### Commands

There is a set of special commands that only work in the web version:

| Command | Action |
|---------|--------|
| `clear` | Clear screen |

### Key bindings

In interactive command-line mode, you can use the following key bindings. Most importantly,
`Tab` for auto-completion, arrow keys and `Ctrl-R` for browsing the command history, and
`Ctrl-D` for exiting the interactive session.

| Key sequence | Action |
|--------------|--------|
| `Tab` | Auto-completion |
| `Ctrl`-`L` | Clear screen |
| `Up`, `Down` | Browse command history |
| `Ctrl`-`R` | Search command history |
| `Ctrl`-`C` | Clear the current line |
| `Shift`-`Enter` | Insert newline |
| `Home`, `Ctrl`-`A` | Move cursor to the beginning of the line |
| `End`, `Ctrl`-`E` | Move cursor to the end of the line |
| `Ctrl`-`Left`, `Ctrl`-`Right` | Move cursor one word left/right |
| `Ctrl`-`K` | Remove text to the right of the cursor |
| `Ctrl`-`U` | Remove text to the left of the cursor |
