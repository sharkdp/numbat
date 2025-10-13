# Customization

## Startup

By default, Numbat will load the following modules/files during startup, in order:

- Numbat Prelude (a module called `prelude`, either from `<module-path>/prelude.nbt` if available, or the builtin version)
- The user initialization file, if available (a file called `init.nbt` from `<config-path>/init.nbt`)

### Config path

Numbat's configuration folder (`<config-path>` above) can be found under:

|Platform|Path|
|---|---|
|Linux|`$HOME/.config/numbat` or `$XDG_CONFIG_HOME/numbat`|
|macOS|`$HOME/Library/Application Support/numbat`|
|Windows|`C:\Users\Alice\AppData\Roaming\numbat`|

## Module paths

Numbat will load modules from the following sources.
Entries higher up in the list take precedence.

|Location|Description|
|---|---|
|`$NUMBAT_MODULES_PATH`|This environment variable can point to a<br>single directory or contain a `:`-separated<br>list of paths|
|`<config-path>/modules`|User-customized module folder|
|`/usr/share/numbat/modules`|System-wide module folder (Linux and macOS)|
|`C:\Program Files\numbat\modules`|System-wide module folder (Windows)|
|`<builtin>`|Builtin modules inside the `numbat` binary|

Note that the System-location might be different for some installation methods.
Refer to your [package manager for details](./cli-installation.html#guidelines-for-package-maintainers).

## Customization

### Configuration

Numbat's configuration file is called `config.toml`, and it needs to be placed in
`<config-path>` described above (`~/.config/numbat/config.toml` on Linux). You
can generate a default configuration by calling

``` bash
numbat --generate-config
```

The most important fields are:

``` toml
# Controls the welcome message. Can be "long", "short", or "off".
intro-banner = "long"

# Controls the prompt character(s) in the interactive terminal.
prompt = ">>> "

# Whether or not to pretty-print expressions before showing the result.
# Can be "always", "never" or "auto". The latter uses pretty-printing
# only in interactive mode.
pretty-print = "auto"

# Controls the edit mode. Can be "emacs", or "vi".
edit-mode = "emacs"

[exchange-rates]
# When and if to load exchange rates from the European Central Bank for
# currency conversions. Can be "on-startup" to always fetch exchange rates
# in the background when the application is started. With "on-first-use",
# Numbat only fetches exchange rates when they are needed. Exchange rate
# fetching can also be disabled using "never". The latter will lead to
# "unknown identifier" errors when a currency unit is being used.
fetching-policy = "on-startup"
```

### Custom functions, constants, units

If you want to add custom functions, constants, or units to your default environment,
create a `init.nbt` file in your config folder (`~/.config/numbat/init.nbt` on Linux).

### Custom modules

You can also create your own modules that can be loaded on demand. To this end,
create a new file, say `<module-path>/user/finance.nbt` in one of the module folders
(e.g. `~/.config/numbat/modules/custom/finance.nbt` on Linux). This module can then be
loaded using

``` numbat
use custom::finance
```

in your Numbat scripts or in the REPL. You can also load custom modules from `init.nbt`
if you want to have them available all the time.

You can also organize modules into subfolders (e.g. `<module-path>/custom/finance/functions.nbt`).
In that case, you can load them using

``` numbat
use custom::finance::functions
```

In fact, the `custom` folder is just a convention to avoid name clashes with the
[standard library](https://github.com/sharkdp/numbat/tree/master/numbat/modules).
