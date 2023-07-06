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

#[derive(Error, Debug)]
pub enum ResolverError {
    #[error("Unknown module '{0}'.")]
    UnknownModule(ModulePath),

    #[error("{0}")]
    ParseError(ParseError),
}

type Result<T> = std::result::Result<T, ResolverError>;

pub(crate) struct Resolver<'a> {
    importer: &'a dyn ModuleImporter,
}

impl<'a> Resolver<'a> {
    pub(crate) fn new(importer: &'a dyn ModuleImporter) -> Self {
        Self { importer }
    }

    fn parse(&self, code: &str) -> Result<Vec<Statement>> {
        parse(code).map_err(ResolverError::ParseError)
    }

    fn inlining_pass(&self, program: &[Statement]) -> Result<(Vec<Statement>, bool)> {
        let mut new_program = vec![];
        let mut performed_imports = false;

        for statement in program {
            match statement {
                Statement::ModuleImport(module_path) => {
                    if let Some(code) = self.importer.import(module_path) {
                        for statement in parse(&code).unwrap() {
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

    pub fn resolve(&self, code: &str) -> Result<Vec<Statement>> {
        // TODO: handle cyclic dependencies & infinite loops

        let mut statements = self.parse(code)?;

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
    fn import(&self, path: &ModulePath) -> Option<String>;
}

pub struct NullImporter {}

impl ModuleImporter for NullImporter {
    fn import(&self, _: &ModulePath) -> Option<String> {
        None
    }
}

pub struct FileSystemImporter {
    root_paths: Vec<PathBuf>,
}

impl FileSystemImporter {
    pub fn new<P: AsRef<Path>>(root_paths: &[P]) -> Self {
        Self {
            root_paths: root_paths
                .iter()
                .map(|p| p.as_ref().to_path_buf())
                .collect(),
        }
    }
}

impl ModuleImporter for FileSystemImporter {
    fn import(&self, module_path: &ModulePath) -> Option<String> {
        for path in &self.root_paths {
            let mut path = path.clone();
            for part in &module_path.0 {
                path = path.join(part);
            }

            path.set_extension("nbt");

            if let Ok(code) = fs::read_to_string(path) {
                return Some(code);
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
        fn import(&self, path: &ModulePath) -> Option<String> {
            match path {
                ModulePath(p) if p == &["foo", "bar"] => Some("use foo::baz".into()),
                ModulePath(p) if p == &["foo", "baz"] => Some("let a = 1".into()),
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
        let program_inlined = resolver.resolve(program).unwrap();

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
