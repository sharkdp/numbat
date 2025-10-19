use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

use compact_str::ToCompactString;
use rust_embed::RustEmbed;

use crate::resolver::ModulePath;

pub trait ModuleImporter: Send + Sync {
    fn import(&self, path: &ModulePath) -> Option<(String, Option<PathBuf>)>;
    fn list_modules(&self) -> Vec<ModulePath>;
}

#[derive(Debug, Clone, Default)]
pub struct NullImporter {}

impl ModuleImporter for NullImporter {
    fn import(&self, _: &ModulePath) -> Option<(String, Option<PathBuf>)> {
        None
    }

    fn list_modules(&self) -> Vec<ModulePath> {
        vec![]
    }
}

#[derive(Debug, Clone, Default)]
pub struct FileSystemImporter {
    root_paths: Vec<PathBuf>,
}

impl FileSystemImporter {
    pub fn add_path<P: AsRef<Path>>(&mut self, path: P) {
        self.root_paths.push(path.as_ref().to_owned());
    }
}

impl ModuleImporter for FileSystemImporter {
    fn import(&self, module_path: &ModulePath) -> Option<(String, Option<PathBuf>)> {
        for path in &self.root_paths {
            let mut path = path.clone();
            for part in &module_path.0 {
                path = path.join(part);
            }

            path.set_extension("nbt");

            if let Ok(code) = fs::read_to_string(&path) {
                return Some((code, Some(path.to_owned())));
            }
        }

        None
    }

    fn list_modules(&self) -> Vec<ModulePath> {
        use walkdir::WalkDir;
        let mut modules = vec![];
        for root_path in &self.root_paths {
            for entry in WalkDir::new(root_path)
                .follow_links(true)
                .follow_root_links(false)
                .into_iter()
                .flatten()
            {
                let path = entry.path();
                if path.is_file()
                    && path.extension() == Some(OsStr::new("nbt"))
                    && let Ok(relative_path) = path.strip_prefix(root_path)
                {
                    let components = relative_path
                        .components()
                        .map(|c| {
                            c.as_os_str()
                                .to_string_lossy()
                                .trim_end_matches(".nbt")
                                .to_compact_string()
                        })
                        .collect();

                    modules.push(ModulePath(components));
                }
            }
        }
        modules
    }
}

#[derive(RustEmbed)]
#[folder = "$CARGO_MANIFEST_DIR/modules/"]
struct BuiltinAssets;

#[derive(Debug, Clone, Default)]

pub struct BuiltinModuleImporter {}

impl ModuleImporter for BuiltinModuleImporter {
    fn import(&self, module_path: &ModulePath) -> Option<(String, Option<PathBuf>)> {
        let mut path = PathBuf::new();
        for part in &module_path.0 {
            path = path.join(part);
        }

        path.set_extension("nbt");

        BuiltinAssets::get(&path.to_string_lossy())
            .map(|embedded_file| {
                let content = embedded_file.data.into_owned();
                String::from_utf8(content).expect("Numbat modules are properly UTF-8 encoded")
            })
            .map(|content| {
                let user_facing_path = PathBuf::from("<builtin>").join("modules").join(path);
                (content, Some(user_facing_path))
            })
    }

    fn list_modules(&self) -> Vec<ModulePath> {
        BuiltinAssets::iter()
            .map(|path| {
                ModulePath(
                    path.trim_end_matches(".nbt")
                        .split('/')
                        .map(|s| s.to_compact_string())
                        .collect(),
                )
            })
            .collect()
    }
}

pub struct ChainedImporter {
    main: Box<dyn ModuleImporter>,
    fallback: Box<dyn ModuleImporter>,
}

impl ChainedImporter {
    pub fn new(main: Box<dyn ModuleImporter>, fallback: Box<dyn ModuleImporter>) -> Self {
        Self { main, fallback }
    }
}

impl ModuleImporter for ChainedImporter {
    fn import(&self, path: &ModulePath) -> Option<(String, Option<PathBuf>)> {
        if let result @ Some(_) = self.main.import(path) {
            result
        } else {
            self.fallback.import(path)
        }
    }

    fn list_modules(&self) -> Vec<ModulePath> {
        let mut modules = self.main.list_modules();
        modules.extend(self.fallback.list_modules());

        modules.sort();
        modules.dedup();

        modules
    }
}
