use std::{
    fs,
    path::{Path, PathBuf},
};

use rust_embed::RustEmbed;

use crate::resolver::ModulePath;

pub trait ModuleImporter {
    fn import(&self, path: &ModulePath) -> Option<(String, Option<PathBuf>)>;
}

#[derive(Debug, Clone, Default)]
pub struct NullImporter {}

impl ModuleImporter for NullImporter {
    fn import(&self, _: &ModulePath) -> Option<(String, Option<PathBuf>)> {
        None
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
            .map(|content| (content, None))
    }
}

pub struct ChainedImporter {
    main: Box<dyn ModuleImporter + Send>,
    fallback: Box<dyn ModuleImporter + Send>,
}

impl ChainedImporter {
    pub fn new(
        main: Box<dyn ModuleImporter + Send>,
        fallback: Box<dyn ModuleImporter + Send>,
    ) -> Self {
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
}
