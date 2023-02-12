use crate::{
    ast::{Expression, Statement},
    number::Number,
    prefix::Prefix,
    prefix_parser::PrefixParser,
    typed_ast::BinaryOperator,
};

pub struct Transformer {
    prefix_parser: PrefixParser,
}

impl Transformer {
    pub fn new() -> Self {
        Self {
            prefix_parser: PrefixParser::new(),
        }
    }

    fn transform_statement(&mut self, statement: &Statement) -> Statement {
        match statement {
            statement @ Statement::Expression(Expression::Identifier(identifier)) => {
                if let (Some(Prefix::Decimal(decimal_prefix)), unit_name) =
                    self.prefix_parser.parse(&identifier)
                {
                    Statement::Expression(Expression::BinaryOperator(
                        BinaryOperator::Mul,
                        Box::new(Expression::Scalar(Number::from_f64(
                            10.0f64.powi(decimal_prefix),
                        ))),
                        Box::new(Expression::Identifier(unit_name)),
                    ))
                } else {
                    statement.clone()
                }
            }
            statement @ Statement::DeclareBaseUnit(name, _) => {
                self.prefix_parser.add_prefixable_unit(name, "TODO");
                statement.clone()
            }
            statement @ Statement::DeclareDerivedUnit(name, _, _) => {
                self.prefix_parser.add_prefixable_unit(name, "TODO");
                statement.clone()
            }
            statement => statement.clone(),
        }
    }

    pub fn transform(&mut self, statements: &[Statement]) -> Vec<Statement> {
        statements
            .iter()
            .map(|statement| self.transform_statement(statement))
            .collect()
    }
}
