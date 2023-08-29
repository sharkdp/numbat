use std::{
    fs,
    path::{Path, PathBuf},
};

use crate::{ast::Statement, parser::parse, span::Span, ParseError};

use codespan_reporting::files::SimpleFiles;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModulePath(pub Vec<String>);

impl std::fmt::Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", itertools::join(self.0.iter().cloned(), "::"))
    }
}

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

    #[error("{0}")]
    ParseError(ParseError),
}

type Result<T> = std::result::Result<T, ResolverError>;

pub(crate) struct Resolver {
    importer: Box<dyn ModuleImporter + Send>,
    pub files: SimpleFiles<String, String>,
    text_code_source_count: usize,
    internal_code_source_count: usize,
    imported_modules: Vec<ModulePath>,
}

impl Resolver {
    pub(crate) fn new(importer: impl ModuleImporter + Send + 'static) -> Self {
        Self {
            importer: Box::new(importer),
            files: SimpleFiles::new(),
            text_code_source_count: 0,
            internal_code_source_count: 0,
            imported_modules: vec![],
        }
    }

    fn add_code_source(&mut self, code_source: CodeSource, content: &str) -> usize {
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

        self.files.add(code_source_name, content.to_string())
    }

    fn parse(&self, code: &str, code_source_id: usize) -> Result<Vec<Statement>> {
        parse(code, code_source_id).map_err(ResolverError::ParseError)
    }

    fn inlining_pass(&mut self, program: &[Statement]) -> Result<Vec<Statement>> {
        let mut new_program = vec![];

        for statement in program {
            match statement {
                Statement::ModuleImport(span, module_path) => {
                    if !self.imported_modules.contains(module_path) {
                        self.imported_modules.push(module_path.clone());
                        if let Some((code, filesystem_path)) = self.importer.import(module_path) {
                            let code_source_id = self.add_code_source(
                                CodeSource::Module(module_path.clone(), filesystem_path),
                                &code,
                            );

                            let imported_program = self.parse(&code, code_source_id)?;
                            let inlined_program = self.inlining_pass(&imported_program)?;
                            for statement in inlined_program {
                                new_program.push(statement);
                            }
                        } else {
                            return Err(ResolverError::UnknownModule(*span, module_path.clone()));
                        }
                    }
                }
                statement => new_program.push(statement.clone()),
            }
        }

        Ok(new_program)
    }

    pub fn resolve(&mut self, code: &str, code_source: CodeSource) -> Result<Vec<Statement>> {
        let code_source_id = self.add_code_source(code_source, code);
        let statements = self.parse(code, code_source_id)?;

        self.inlining_pass(&statements)
    }
}

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

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, Statement},
        number::Number,
    };

    use super::*;

    struct TestImporter {}

    impl ModuleImporter for TestImporter {
        fn import(&self, path: &ModulePath) -> Option<(String, Option<PathBuf>)> {
            match path {
                ModulePath(p) if p == &["foo", "bar"] => Some(("use foo::baz".into(), None)),
                ModulePath(p) if p == &["foo", "baz"] => Some(("let a = 1".into(), None)),
                // ----
                ModulePath(p) if p == &["mod_a"] => Some(("use mod_b".into(), None)),
                ModulePath(p) if p == &["mod_b"] => Some(("use mod_c\n let x = y".into(), None)),
                ModulePath(p) if p == &["mod_c"] => Some(("let y = 1".into(), None)),
                // ----
                ModulePath(p) if p == &["cycle_a"] => Some(("use cycle_b".into(), None)),
                ModulePath(p) if p == &["cycle_b"] => Some(("use cycle_a".into(), None)),
                _ => None,
            }
        }
    }

    #[test]
    fn resolver_basic_import() {
        use crate::ast::ReplaceSpans;

        let program = "
        use foo::bar
        a
        ";

        let importer = TestImporter {};

        let mut resolver = Resolver::new(importer);
        let program_inlined = resolver.resolve(program, CodeSource::Internal).unwrap();

        assert_eq!(
            &program_inlined.replace_spans(),
            &[
                Statement::DefineVariable {
                    identifier_span: Span::dummy(),
                    identifier: "a".into(),
                    expr: Expression::Scalar(Span::dummy(), Number::from_f64(1.0)),
                    type_annotation: None
                },
                Statement::Expression(Expression::Identifier(Span::dummy(), "a".into()))
            ]
        );
    }

    #[test]
    fn resolver_repeated_includes() {
        use crate::ast::ReplaceSpans;

        let program = "
        use foo::bar
        use foo::bar
        a
        ";

        let importer = TestImporter {};

        let mut resolver = Resolver::new(importer);
        let program_inlined = resolver.resolve(program, CodeSource::Internal).unwrap();

        assert_eq!(
            &program_inlined.replace_spans(),
            &[
                Statement::DefineVariable {
                    identifier_span: Span::dummy(),
                    identifier: "a".into(),
                    expr: Expression::Scalar(Span::dummy(), Number::from_f64(1.0)),
                    type_annotation: None
                },
                Statement::Expression(Expression::Identifier(Span::dummy(), "a".into()))
            ]
        );
    }

    #[test]
    fn resolver_depth_first_includes() {
        use crate::ast::ReplaceSpans;

        let program = "
        use mod_a
        use mod_c
        ";

        let importer = TestImporter {};

        let mut resolver = Resolver::new(importer);
        let program_inlined = resolver.resolve(program, CodeSource::Internal).unwrap();

        assert_eq!(
            &program_inlined.replace_spans(),
            &[
                Statement::DefineVariable {
                    identifier_span: Span::dummy(),
                    identifier: "y".into(),
                    expr: Expression::Scalar(Span::dummy(), Number::from_f64(1.0)),
                    type_annotation: None
                },
                Statement::DefineVariable {
                    identifier_span: Span::dummy(),
                    identifier: "x".into(),
                    expr: Expression::Identifier(Span::dummy(), "y".into()),
                    type_annotation: None
                },
            ]
        );
    }

    #[test]
    fn resolved_cyclic_imports() {
        let program = "
        use cycle_a
        ";

        let importer = TestImporter {};

        let mut resolver = Resolver::new(importer);
        let program_inlined = resolver.resolve(program, CodeSource::Internal).unwrap();

        assert_eq!(&program_inlined, &[]);
    }
}
