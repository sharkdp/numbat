use std::{
    collections::HashMap,
    path::PathBuf,
    pin::Pin,
    str::FromStr,
    sync::atomic::{AtomicUsize, Ordering},
};

use numbat::{
    Transformer, ast,
    compact_str::ToCompactString,
    module_importer::{BuiltinAssets, FileSystemImporter, ModuleImporter},
    resolver::ModulePath,
    typechecker::TypeChecker,
};
use tempfile::TempDir;
use tower_lsp_server::{UriExt, lsp_types::*};

use crate::backend::{parse_error_to_diag, span_to_range};

#[derive(Debug)]
pub struct File {
    // Location of the file on disk
    pub uri: Uri,
    // Location of the module in code
    pub module: ModulePath,
    // Code source id
    pub code_source_id: usize,

    // The statements references the content which means:
    // 1. The content cannot be moved
    // 2. The statements must be dropped BEFORE the content
    pub content: Pin<String>,
    pub ast_original_stmts: Vec<ast::Statement<'static>>,
    pub ast_transformed_stmts: Vec<ast::Statement<'static>>,
    pub typed_stmts: Vec<numbat::Statement<'static>>,
    pub diags: Vec<Diagnostic>,

    // Cached info
    pub transformer: numbat::Transformer,
    pub type_checker: numbat::typechecker::TypeChecker,
}

impl Drop for File {
    fn drop(&mut self) {
        // Safety: We must drop the statements before the content is dropped
        let stmts = std::mem::take(&mut self.ast_original_stmts);
        drop(stmts);
        let stmts = std::mem::take(&mut self.ast_transformed_stmts);
        drop(stmts);
        let stmts = std::mem::take(&mut self.typed_stmts);
        drop(stmts);
    }
}

#[derive(Debug, Default)]
pub struct FileMapping {
    pub files: HashMap<Uri, File>,
    pub mod_to_uri: HashMap<ModulePath, Uri>,

    // Mapping between the codesource id and the uri
    pub uri_to_cs_id: HashMap<Uri, usize>,
    pub cs_id_to_uri: HashMap<usize, Uri>,
    pub current_cs_id: AtomicUsize,

    // Tempdir holding the prelude
    pub prelude_dir: Option<TempDir>,
    // Conf to import files
    pub fs_importer: FileSystemImporter,
}

impl FileMapping {
    /// Set up the file mapper with the prelude already loaded.
    /// The prelude is created in a temporary directory that should be removed
    /// automatically once the lsp is closed.
    pub fn with_prelude() -> FileMapping {
        let mut this = Self::default();

        let mut fs_importer = FileSystemImporter::default();
        for path in numbat_cli_helpers::get_modules_paths() {
            fs_importer.add_path(path);
        }
        this.fs_importer = fs_importer;

        let tempdir = TempDir::new().expect("Can't create temporary directory");
        this.prelude_dir = Some(tempdir);
        let base_path = this.prelude_dir.as_ref().unwrap().path().to_path_buf();

        // We can't import the module in any random order since they depends on
        // each other, so we'll have to build a dependency graph first
        let mut dependency_graph: HashMap<ModulePath, Vec<ModulePath>> = HashMap::new();

        // We're going to load and write all the prelude to disk so we're sure
        // it's available to the text editor.
        for filename in BuiltinAssets::iter() {
            let file_path = PathBuf::from_str(&filename).unwrap();
            let module: Vec<_> = file_path
                .components()
                .map(|c| {
                    c.as_os_str()
                        .to_str()
                        .unwrap()
                        .trim_end_matches(".nbt")
                        .to_compact_string()
                })
                .collect();
            let module = ModulePath(module);
            // 1. Create the file on disk to we can go to definition
            let content = BuiltinAssets::get(&filename).unwrap();
            let tmp_path = base_path.join(filename.as_ref());
            std::fs::create_dir_all(tmp_path.parent().unwrap()).unwrap();
            std::fs::write(&tmp_path, content.data.as_ref()).unwrap();
            let content = String::from_utf8(content.data.to_vec()).unwrap();

            let deps = dependency_graph.entry(module.clone()).or_default();
            // prelude can't fail parsing
            let ast_stmts = numbat::parse(&content, 0).unwrap();
            for stmt in ast_stmts {
                if let ast::Statement::ModuleImport(_, other_module) = stmt {
                    deps.push(other_module.to_owned());
                }
            }
        }

        // at most we'll loop as many time as we have files in the prelude
        for _ in 0..dependency_graph.len() {
            // the list of entry to remove from the dependency graph at the end
            // of the loop.
            let mut to_delete = Vec::new();
            for (module, deps) in dependency_graph.iter_mut() {
                let mut deps_to_delete = Vec::new();
                for (i, dep) in deps.iter().enumerate() {
                    if this.mod_to_uri.contains_key(dep) {
                        deps_to_delete.push(i);
                    }
                }
                // we remove the idx from the end to be sure we don't invalidate
                // our indexes ourselves
                for to_del in deps_to_delete.iter().rev() {
                    deps.swap_remove(*to_del);
                }
                // It's safe to import the module
                if deps.is_empty() {
                    to_delete.push(module.to_owned());
                    let mut path = base_path.clone();
                    for nibble in module.0.iter() {
                        path.push(nibble);
                    }
                    assert!(path.set_extension("nbt"));
                    let uri = Uri::from_file_path(path).unwrap();
                    this.load_module(
                        module.to_owned(),
                        Some(uri.path().as_str()),
                        None,
                        &mut HashMap::new(),
                    );
                }
            }

            for delete in to_delete {
                dependency_graph.remove(&delete);
            }

            // No more work to do
            if dependency_graph.is_empty() {
                break;
            }
        }
        assert!(
            dependency_graph.is_empty(),
            "Dependency graph must be empty at the end of the import but was left with" // {dependency_graph:#?}"
        );

        this
    }

    #[allow(clippy::mutable_key_type)]
    pub fn load_module(
        &mut self,
        module_path: ModulePath,
        path: Option<&str>,
        content: Option<String>,
        diags_to_publish: &mut HashMap<Uri, Vec<Diagnostic>>,
        // we can't return the loaded &File directly because of https://docs.rs/polonius-the-crab/latest/polonius_the_crab/
    ) -> Option<Uri> {
        if content.is_none()
            && let Some(uri) = self.mod_to_uri.get(&module_path)
        {
            return Some(uri.clone());
        }

        let mut cs_id = None;

        let (content, uri) = match path {
            Some(path) => {
                let uri = Uri::from_file_path(path)?;
                if let Some(file) = self.files.get_mut(&uri) {
                    if Some(&*file.content) == content.as_deref() || content.is_none() {
                        return Some(uri);
                    }
                    // we're updating the content of our file
                    cs_id = Some(file.code_source_id);
                    // we want to make sure we won't update the content _before_ dropping the stmts
                    let _ = std::mem::take(&mut file.ast_original_stmts);
                    let _ = std::mem::take(&mut file.ast_transformed_stmts);
                    let _ = std::mem::take(&mut file.typed_stmts);
                }
                let content = match content {
                    Some(content) => content,
                    None => std::fs::read_to_string(path).ok()?,
                };
                (content, uri)
            }
            None => {
                // If the module is not already part of our files we must load it
                // from disk.
                // The prelude has already been loaded on initialization so we know
                // for sure we must look into the fs_importer.
                let (content, path) = self.fs_importer.import(&module_path)?;
                let path = path.expect("Filesystem importer must return a path");
                let uri = Uri::from_file_path(&path)
                    .expect("The file exists since we were able to import it");
                (content, uri)
            }
        };
        let cs_id = match cs_id {
            Some(id) => id,
            None => self.current_cs_id.fetch_add(1, Ordering::Relaxed),
        };
        let content = Pin::new(content);

        // The list of diagnostic local to the file we're going to load.
        // They must be published at the end of the function.
        let mut diags = Vec::new();

        let ast_stmts = match numbat::parse(&content, cs_id) {
            Ok(stmts) => stmts,
            Err((stmts, errors)) => {
                diags.extend(errors.iter().map(|e| parse_error_to_diag(e, &content)));
                stmts
            }
        };
        let ast_original_stmts: Vec<ast::Statement<'static>> =
            unsafe { std::mem::transmute(ast_stmts) };
        let mut transformer = Transformer::new();
        let mut ast_transformed_stmts: Vec<ast::Statement<'static>> = Vec::new();
        for stmt in ast_original_stmts.iter() {
            let mut stmt = stmt.clone();
            if let ast::Statement::ModuleImport(span, module_path) = &stmt {
                let module_path = ModulePath(
                    module_path
                        .0
                        .iter()
                        .map(|s| s.to_compact_string())
                        .collect(),
                );
                match self.load_module(module_path.clone(), None, None, diags_to_publish) {
                    None => {
                        // we can't do the import so we drop it,
                        // log an error and continue the transform.
                        diags.push(Diagnostic {
                            range: span_to_range(*span, &content),
                            severity: Some(DiagnosticSeverity::ERROR),
                            code: None,
                            code_description: None,
                            source: Some(String::from("Importer")),
                            message: format!("Could not import module {module_path}"),
                            related_information: None,
                            tags: None,
                            data: None,
                        });
                        continue;
                    }
                    Some(uri) => {
                        let file = self.files.get(&uri).unwrap();
                        // We must merge our env with the one in the file in
                        // case it's going to cause name clashes
                        transformer.merge_with(&file.transformer);
                    }
                }
            };
            match transformer.transform_statement(&mut stmt) {
                Ok(()) => ast_transformed_stmts.push(stmt),
                Err(e) => diags.append(&mut self.name_resolution_error_to_diag(&content, &e)),
            };
        }

        let mut type_checker = TypeChecker::default();
        let mut typed_stmts = vec![];

        for statement in ast_transformed_stmts.iter() {
            if let ast::Statement::ModuleImport(_span, module_path) = statement {
                // we must load the typechecker state of the other module
                let module_path = ModulePath(
                    module_path
                        .0
                        .iter()
                        .map(|s| s.to_compact_string())
                        .collect(),
                );
                let uri = self
                    .load_module(module_path.clone(), None, None, diags_to_publish)
                    .expect("Module have already been loaded in previous phase");
                let file = self.files.get(&uri).unwrap();
                type_checker.merge_with(&file.type_checker);
            } else {
                match type_checker.check_statement(statement) {
                    Ok(stmt) => typed_stmts.push(stmt),
                    Err(error) => {
                        let mut d = self.type_check_error_to_diag(&content, &error);
                        diags.append(&mut d);
                    }
                }
            }
        }

        // Publish the diagnostics
        diags_to_publish
            .entry(uri.clone())
            .or_default()
            .extend_from_slice(&diags);

        // Finally, cache everything
        let file = File {
            uri: uri.clone(),
            module: module_path.clone(),
            code_source_id: cs_id,
            content,
            ast_original_stmts,
            ast_transformed_stmts,
            typed_stmts,
            diags,
            transformer,
            type_checker,
        };
        self.files.insert(uri.clone(), file);
        self.mod_to_uri.insert(module_path, uri.clone());
        self.uri_to_cs_id.insert(uri.clone(), cs_id);
        self.cs_id_to_uri.insert(cs_id, uri.clone());

        Some(uri)
    }

    pub fn uri_to_module(uri: &Uri) -> ModulePath {
        let path = uri.to_file_path().unwrap();
        let components = path
            .components()
            .map(|c| {
                c.as_os_str()
                    .to_string_lossy()
                    .trim_end_matches(".nbt")
                    .to_compact_string()
            })
            .collect();
        ModulePath(components)
    }
}
