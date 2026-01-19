use std::{collections::HashMap, path::PathBuf, sync::Arc};

use crate::{
    ParseError,
    ast::{ImportKind, Statement, Visibility},
    module_importer::ModuleImporter,
    parser::parse,
    span::Span,
};

use codespan_reporting::files::SimpleFiles;
use compact_str::{CompactString, ToCompactString};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModulePath(pub Vec<CompactString>);

impl std::fmt::Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", itertools::join(self.0.iter().cloned(), "::"))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModulePathBorrowed<'a>(pub Vec<&'a str>);

#[derive(Debug, Clone)]
pub enum CodeSource {
    /// User input from the command line or a REPL
    Text,

    /// For internal use, e.g. when executing special statements (like "use prelude")
    /// during startup.
    Internal,

    /// A file that has been read in
    File(PathBuf),

    /// A module that has been imported
    Module(ModulePath, Option<PathBuf>),
}

#[derive(Error, Clone, Debug)]
pub enum ResolverError {
    #[error("Unknown module '{1}'.")]
    UnknownModule(Span, ModulePath),

    #[error("Cyclic import detected: module '{1}' is already being imported")]
    CyclicImport(Span, ModulePath),

    #[error("Item '{item}' not found in module '{module}'.")]
    ItemNotFound {
        span: Span,
        module: ModulePath,
        item: String,
    },

    #[error("Cannot import private item '{item}' from module '{module}'.")]
    PrivateItem {
        span: Span,
        module: ModulePath,
        item: String,
    },

    #[error("{}", .0.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n"))]
    ParseErrors(Vec<ParseError>),
}

type Result<T> = std::result::Result<T, ResolverError>;

#[derive(Clone)]
pub struct Resolver {
    importer: Arc<dyn ModuleImporter>,
    pub files: SimpleFiles<String, String>,
    text_code_source_count: usize,
    internal_code_source_count: usize,
    pub imported_modules: Vec<ModulePath>,
    /// Modules currently being imported (for cycle detection)
    import_stack: Vec<ModulePath>,
    codesources: HashMap<usize, CodeSource>,
}

impl Resolver {
    pub(crate) fn new(importer: impl ModuleImporter + 'static) -> Self {
        Self {
            importer: Arc::new(importer),
            files: SimpleFiles::new(),
            text_code_source_count: 0,
            internal_code_source_count: 0,
            imported_modules: vec![],
            import_stack: vec![],
            codesources: HashMap::new(),
        }
    }

    pub fn add_code_source(&mut self, code_source: CodeSource, content: &str) -> usize {
        let code_source_name = match &code_source {
            CodeSource::Text => {
                self.text_code_source_count += 1;
                format!("<input:{}>", self.text_code_source_count)
            }
            CodeSource::Internal => {
                self.internal_code_source_count += 1;
                format!("<internal:{}>", self.internal_code_source_count)
            }
            CodeSource::File(path) => format!("File {}", path.to_string_lossy()),
            CodeSource::Module(module_path, path) => format!(
                "Module '{module_path}', File {path}",
                module_path = itertools::join(module_path.0.iter(), "::"),
                path = path
                    .as_ref()
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or("?".into()),
            ),
        };

        let id = self.files.add(code_source_name, content.to_string());
        self.codesources.insert(id, code_source);

        id
    }

    pub fn get_code_source(&self, id: usize) -> CodeSource {
        self.codesources.get(&id).cloned().unwrap()
    }

    /// Get the module path for a given code source ID, if it came from a module import.
    pub fn get_module_path(&self, id: usize) -> Option<ModulePath> {
        match self.codesources.get(&id) {
            Some(CodeSource::Module(path, _)) => Some(path.clone()),
            _ => None,
        }
    }

    /// Get all code_source_ids that correspond to module imports.
    pub fn get_module_source_ids(&self) -> std::collections::HashSet<usize> {
        self.codesources
            .iter()
            .filter_map(|(id, cs)| matches!(cs, CodeSource::Module(_, _)).then_some(*id))
            .collect()
    }

    fn parse<'a>(&self, code: &'a str, code_source_id: usize) -> Result<Vec<Statement<'a>>> {
        parse(code, code_source_id).map_err(|e| ResolverError::ParseErrors(e.1))
    }

    fn inlining_pass<'a>(&mut self, program: &[Statement<'a>]) -> Result<Vec<Statement<'a>>> {
        let mut new_program = vec![];

        for statement in program {
            match statement {
                Statement::ModuleImport(span, ModulePathBorrowed(module_path), import_kind) => {
                    let module_path =
                        ModulePath(module_path.iter().map(|s| s.to_compact_string()).collect());

                    // Check for cyclic import
                    if self.import_stack.contains(&module_path) {
                        return Err(ResolverError::CyclicImport(*span, module_path));
                    }

                    // Skip if already imported (for glob imports)
                    // For selective imports, we still need to process each time to ensure the item is available
                    if matches!(import_kind, ImportKind::Glob)
                        && self.imported_modules.contains(&module_path)
                    {
                        continue;
                    }

                    if let Some((code, filesystem_path)) = self.importer.import(&module_path) {
                        let code: &'static str = Box::leak(code.to_string().into_boxed_str());

                        // Push to import stack before processing
                        self.import_stack.push(module_path.clone());

                        let code_source_id = self.add_code_source(
                            CodeSource::Module(module_path.clone(), filesystem_path),
                            code,
                        );

                        let imported_program = self.parse(code, code_source_id)?;
                        let inlined_program = self.inlining_pass(&imported_program)?;

                        // Pop from import stack after successful processing
                        self.import_stack.pop();

                        // Mark as fully imported (for glob imports)
                        if matches!(import_kind, ImportKind::Glob) {
                            self.imported_modules.push(module_path.clone());
                        }

                        // Handle import based on kind
                        // NOTE: We include ALL statements (public and private) here.
                        // Visibility is checked later in the typechecker, which allows
                        // public functions to call private helpers within the same module.
                        match import_kind {
                            ImportKind::Glob => {
                                // For glob imports, include all statements from the module
                                new_program.extend(inlined_program);
                            }
                            ImportKind::Item(item_name) => {
                                // For selective imports, verify the item exists and is public,
                                // but still include all statements for module scoping
                                let mut found = false;
                                let mut is_private = false;

                                for statement in &inlined_program {
                                    if let Some((name, visibility)) = statement.definition_info() {
                                        if name == *item_name {
                                            found = true;
                                            if visibility == Visibility::Private {
                                                is_private = true;
                                            }
                                            break;
                                        }
                                    }
                                }

                                if !found {
                                    return Err(ResolverError::ItemNotFound {
                                        span: *span,
                                        module: module_path,
                                        item: item_name.to_string(),
                                    });
                                }

                                if is_private {
                                    return Err(ResolverError::PrivateItem {
                                        span: *span,
                                        module: module_path,
                                        item: item_name.to_string(),
                                    });
                                }

                                // Include all statements for proper module scoping
                                new_program.extend(inlined_program);
                            }
                        }
                    } else {
                        return Err(ResolverError::UnknownModule(*span, module_path));
                    }
                }
                statement => new_program.push(statement.clone()),
            }
        }

        Ok(new_program)
    }

    pub fn resolve<'a>(
        &mut self,
        code: &'a str,
        code_source: CodeSource,
    ) -> Result<Vec<Statement<'a>>> {
        let code_source_id = self.add_code_source(code_source, code);
        let statements = self.parse(code, code_source_id)?;

        self.inlining_pass(&statements)
    }

    pub fn get_importer(&self) -> &dyn ModuleImporter {
        self.importer.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{DefineVariable, Expression, Visibility},
        number::Number,
    };

    use super::*;

    struct TestImporter {}

    impl ModuleImporter for TestImporter {
        fn import(&self, path: &ModulePath) -> Option<(String, Option<PathBuf>)> {
            match path {
                ModulePath(p) if p == &["foo", "bar"] => Some(("use foo::baz::*".into(), None)),
                ModulePath(p) if p == &["foo", "baz"] => Some(("pub let a = 1".into(), None)),
                // ----
                ModulePath(p) if p == &["mod_a"] => Some(("use mod_b::*".into(), None)),
                ModulePath(p) if p == &["mod_b"] => {
                    Some(("use mod_c::*\n pub let x = y".into(), None))
                }
                ModulePath(p) if p == &["mod_c"] => Some(("pub let y = 1".into(), None)),
                // ----
                ModulePath(p) if p == &["cycle_a"] => Some(("use cycle_b::*".into(), None)),
                ModulePath(p) if p == &["cycle_b"] => Some(("use cycle_a::*".into(), None)),
                _ => None,
            }
        }

        fn list_modules(&self) -> Vec<ModulePath> {
            unimplemented!()
        }
    }

    #[test]
    fn resolver_basic_import() {
        use crate::ast::ReplaceSpans;

        let program = "
        use foo::bar::*
        a
        ";

        let importer = TestImporter {};

        let mut resolver = Resolver::new(importer);
        let program_inlined = resolver.resolve(program, CodeSource::Internal).unwrap();

        assert_eq!(
            &program_inlined.replace_spans(),
            &[
                Statement::DefineVariable(DefineVariable {
                    identifier_span: Span::dummy(),
                    identifier: "a",
                    expr: Expression::Scalar(Span::dummy(), Number::from_f64(1.0)),
                    type_annotation: None,
                    decorators: Vec::new(),
                    visibility: Visibility::Public,
                }),
                Statement::Expression(Expression::Identifier(Span::dummy(), "a"))
            ]
        );
    }

    #[test]
    fn resolver_repeated_includes() {
        use crate::ast::ReplaceSpans;

        let program = "
        use foo::bar::*
        use foo::bar::*
        a
        ";

        let importer = TestImporter {};

        let mut resolver = Resolver::new(importer);
        let program_inlined = resolver.resolve(program, CodeSource::Internal).unwrap();

        assert_eq!(
            &program_inlined.replace_spans(),
            &[
                Statement::DefineVariable(DefineVariable {
                    identifier_span: Span::dummy(),
                    identifier: "a",
                    expr: Expression::Scalar(Span::dummy(), Number::from_f64(1.0)),
                    type_annotation: None,
                    decorators: Vec::new(),
                    visibility: Visibility::Public,
                }),
                Statement::Expression(Expression::Identifier(Span::dummy(), "a"))
            ]
        );
    }

    #[test]
    fn resolver_depth_first_includes() {
        use crate::ast::ReplaceSpans;

        let program = "
        use mod_a::*
        use mod_c::*
        ";

        let importer = TestImporter {};

        let mut resolver = Resolver::new(importer);
        let program_inlined = resolver.resolve(program, CodeSource::Internal).unwrap();

        assert_eq!(
            &program_inlined.replace_spans(),
            &[
                Statement::DefineVariable(DefineVariable {
                    identifier_span: Span::dummy(),
                    identifier: "y",
                    expr: Expression::Scalar(Span::dummy(), Number::from_f64(1.0)),
                    type_annotation: None,
                    decorators: Vec::new(),
                    visibility: Visibility::Public,
                }),
                Statement::DefineVariable(DefineVariable {
                    identifier_span: Span::dummy(),
                    identifier: "x",
                    expr: Expression::Identifier(Span::dummy(), "y"),
                    type_annotation: None,
                    decorators: Vec::new(),
                    visibility: Visibility::Public,
                }),
            ]
        );
    }

    #[test]
    fn resolver_detects_cyclic_imports() {
        let program = "
        use cycle_a::*
        ";

        let importer = TestImporter {};

        let mut resolver = Resolver::new(importer);
        let result = resolver.resolve(program, CodeSource::Internal);

        assert!(matches!(result, Err(ResolverError::CyclicImport(_, _))));
    }

    struct SelectiveImporter {}

    impl ModuleImporter for SelectiveImporter {
        fn import(&self, path: &ModulePath) -> Option<(String, Option<PathBuf>)> {
            match path {
                ModulePath(p) if p == &["mymod"] => Some((
                    "pub let public_var = 1\nlet private_var = 2\npub fn public_fn() = 3".into(),
                    None,
                )),
                _ => None,
            }
        }

        fn list_modules(&self) -> Vec<ModulePath> {
            unimplemented!()
        }
    }

    #[test]
    fn resolver_selective_import_public_item() {
        let program = "use mymod::public_var";

        let importer = SelectiveImporter {};
        let mut resolver = Resolver::new(importer);
        let program_inlined = resolver.resolve(program, CodeSource::Internal).unwrap();

        // Selective imports now include all statements from the module for proper scoping.
        // The resolver verifies the requested item exists and is public, but all items
        // are included so public functions can call private helpers.
        // Visibility is enforced later in the typechecker.
        assert_eq!(program_inlined.len(), 3); // public_var, private_var, public_fn

        // Verify the requested item is included
        let names: Vec<_> = program_inlined
            .iter()
            .filter_map(|s| s.definition_info().map(|(name, _)| name))
            .collect();
        assert!(names.contains(&"public_var"));
    }

    #[test]
    fn resolver_selective_import_private_item_fails() {
        let program = "use mymod::private_var";

        let importer = SelectiveImporter {};
        let mut resolver = Resolver::new(importer);
        let result = resolver.resolve(program, CodeSource::Internal);

        assert!(matches!(result, Err(ResolverError::PrivateItem { .. })));
    }

    #[test]
    fn resolver_selective_import_nonexistent_item_fails() {
        let program = "use mymod::nonexistent";

        let importer = SelectiveImporter {};
        let mut resolver = Resolver::new(importer);
        let result = resolver.resolve(program, CodeSource::Internal);

        assert!(matches!(result, Err(ResolverError::ItemNotFound { .. })));
    }

    #[test]
    fn resolver_glob_import_includes_all_for_module_scoping() {
        let program = "use mymod::*";

        let importer = SelectiveImporter {};
        let mut resolver = Resolver::new(importer);
        let program_inlined = resolver.resolve(program, CodeSource::Internal).unwrap();

        // All items are included for proper module scoping (public functions can call private helpers).
        // Visibility is checked later in the typechecker.
        assert_eq!(program_inlined.len(), 3);

        let names: Vec<_> = program_inlined
            .iter()
            .filter_map(|s| s.definition_info().map(|(name, _)| name))
            .collect();
        assert!(names.contains(&"public_var"));
        assert!(names.contains(&"public_fn"));
        assert!(names.contains(&"private_var")); // Included for module scoping
    }
}
