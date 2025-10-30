use std::path::PathBuf;

pub fn get_modules_paths() -> Vec<PathBuf> {
    let mut paths = vec![];

    if let Some(modules_path) = std::env::var_os("NUMBAT_MODULES_PATH") {
        for path in modules_path.to_string_lossy().split(':') {
            paths.push(path.into());
        }
    }

    paths.push(get_config_path().join("modules"));

    // We read the value of this environment variable at compile time to
    // allow package maintainers to control the system-wide module path
    // for Numbat.
    if let Some(system_module_path) = option_env!("NUMBAT_SYSTEM_MODULE_PATH") {
        if !system_module_path.is_empty() {
            paths.push(system_module_path.into());
        }
    } else if cfg!(unix) {
        paths.push("/usr/share/numbat/modules".into());
    } else {
        paths.push("C:\\Program Files\\numbat\\modules".into());
    }
    paths
}

pub fn get_config_path() -> PathBuf {
    let config_dir = dirs::config_dir().unwrap_or_else(|| PathBuf::from("."));
    config_dir.join("numbat")
}
