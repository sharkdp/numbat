use std::{
    fs,
    path::{Path, PathBuf},
};

use crate::{ast::Statement, parser::parse, ParseError};

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
    #[error("Unknown module '{0}'.")]
    UnknownModule(ModulePath),

    #[error("{inner}")]
    ParseError {
        inner: ParseError,
        code_source: CodeSource,
    },
}

type Result<T> = std::result::Result<T, ResolverError>;

pub(crate) struct Resolver<'a> {
    importer: &'a dyn ModuleImporter,
}

impl<'a> Resolver<'a> {
    pub(crate) fn new(importer: &'a dyn ModuleImporter) -> Self {
        Self { importer }
    }

    fn parse(&self, code: &str, code_source: CodeSource) -> Result<Vec<Statement>> {
        parse(code).map_err(|inner| ResolverError::ParseError { inner, code_source })
    }

    fn inlining_pass(&self, program: &[Statement]) -> Result<(Vec<Statement>, bool)> {
        let mut new_program = vec![];
        let mut performed_imports = false;

        for statement in program {
            match statement {
                Statement::ModuleImport(module_path) => {
                    if let Some((code, filesystem_path)) = self.importer.import(module_path) {
                        for statement in self.parse(
                            &code,
                            CodeSource::Module(module_path.clone(), filesystem_path),
                        )? {
                            new_program.push(statement);
                        }
                        performed_imports = true;
                    } else {
                        return Err(ResolverError::UnknownModule(module_path.clone()));
                    }
                }
                statement => new_program.push(statement.clone()),
            }
        }

        Ok((new_program, performed_imports))
    }

    pub fn resolve(&self, code: &str, code_source: CodeSource) -> Result<Vec<Statement>> {
        // TODO: handle cyclic dependencies & infinite loops

        let mut statements = self.parse(code, code_source)?;

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

pub struct NullImporter {}

impl NullImporter {
    pub fn new() -> NullImporter {
        Self {}
    }
}

impl ModuleImporter for NullImporter {
    fn import(&self, _: &ModulePath) -> Option<(String, Option<PathBuf>)> {
        None
    }
}

pub struct FileSystemImporter {
    root_paths: Vec<PathBuf>,
}

impl FileSystemImporter {
    pub fn new() -> Self {
        Self { root_paths: vec![] }
    }

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
        let program = "
        use foo::bar
        a
        ";

        let importer = TestImporter {};

        let resolver = Resolver::new(&importer);
        let program_inlined = resolver.resolve(program, CodeSource::Text).unwrap();

        assert_eq!(
            &program_inlined,
            &[
                Statement::DeclareVariable(
                    "a".into(),
                    Expression::Scalar(Number::from_f64(1.0)),
                    None
                ),
                Statement::Expression(Expression::Identifier("a".into()))
            ]
        );
    }
}
