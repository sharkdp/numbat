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

    /// A file that has been read in
    File(PathBuf),

    /// A module that has been imported
    Module(ModulePath, Option<PathBuf>),
}

#[derive(Error, Debug)]
pub enum ResolverError {
    #[error("Unknown module '{1}'.")]
    UnknownModule(Span, ModulePath),

    #[error("{0}")]
    ParseError(ParseError),
}

type Result<T> = std::result::Result<T, ResolverError>;

pub(crate) struct Resolver {
    importer: Box<dyn ModuleImporter>,
    code_sources: Vec<CodeSource>,
    pub files: SimpleFiles<String, String>,
}

impl Resolver {
    pub(crate) fn new(importer: impl ModuleImporter + 'static) -> Self {
        Self {
            importer: Box::new(importer),
            code_sources: vec![],
            files: SimpleFiles::new(),
        }
    }

    fn add_code_source(&mut self, code_source: CodeSource, content: &str) -> usize {
        let code_source_text = match &code_source {
            CodeSource::Text => "<input>".to_string(),
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

        let _file_id = self.files.add(code_source_text, content.to_string()); // TODO: it's a shame we need to clone the full source code here

        self.code_sources.push(code_source);
        self.code_sources.len() - 1
    }

    fn parse(&self, code: &str, code_source_index: usize) -> Result<Vec<Statement>> {
        parse(code, code_source_index).map_err(|inner| ResolverError::ParseError(inner))
    }

    fn inlining_pass(&mut self, program: &[Statement]) -> Result<(Vec<Statement>, bool)> {
        let mut new_program = vec![];
        let mut performed_imports = false;

        for statement in program {
            match statement {
                Statement::ModuleImport(span, module_path) => {
                    if let Some((code, filesystem_path)) = self.importer.import(module_path) {
                        let index = self.add_code_source(
                            CodeSource::Module(module_path.clone(), filesystem_path),
                            &code,
                        );
                        for statement in self.parse(&code, index)? {
                            new_program.push(statement);
                        }
                        performed_imports = true;
                    } else {
                        return Err(ResolverError::UnknownModule(*span, module_path.clone()));
                    }
                }
                statement => new_program.push(statement.clone()),
            }
        }

        Ok((new_program, performed_imports))
    }

    pub fn resolve(&mut self, code: &str, code_source: CodeSource) -> Result<Vec<Statement>> {
        // TODO: handle cyclic dependencies & infinite loops

        let index = self.add_code_source(code_source, code);
        let mut statements = self.parse(code, index)?;

        loop {
            let result = self.inlining_pass(&statements)?;
            statements = result.0;
            if !result.1 {
                return Ok(statements);
            }
        }
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
        let program_inlined = resolver.resolve(program, CodeSource::Text).unwrap();

        assert_eq!(
            &program_inlined.replace_spans(),
            &[
                Statement::DeclareVariable(
                    Span::dummy(),
                    "a".into(),
                    Expression::Scalar(Number::from_f64(1.0)),
                    None
                ),
                Statement::Expression(Expression::Identifier(Span::dummy(), "a".into()))
            ]
        );
    }
}
