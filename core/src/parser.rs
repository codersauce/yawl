use crate::lexer::Token;

pub struct Parser<'a> {
    tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Parser<'a> {
        Self { tokens }
    }

    pub fn parse(&mut self) -> anyhow::Result<Program> {
        let mut statements = Vec::new();

        while self.peek_token().is_some() {
            statements.push(self.parse_statement()?);
        }

        Ok(Program::new(statements))
    }

    fn parse_statement(&mut self) -> anyhow::Result<Statement> {
        Ok(Statement::Expression(self.parse_exp()?))
    }

    fn parse_exp(&mut self) -> anyhow::Result<Exp> {
        let left = self.parse_lhs()?;

        if let Some(op) = self.parse_binary_operator()? {
            let right = self.parse_exp()?;
            return Ok(Exp::Binary(Box::new(left), op, Box::new(right)));
        }

        Ok(left)
    }

    fn parse_binary_operator(&mut self) -> anyhow::Result<Option<BinaryOperator>> {
        let res = match self.peek_token() {
            Some(Token::Plus) => Some(BinaryOperator::Add),
            Some(Token::Minus) => Some(BinaryOperator::Subtract),
            Some(Token::Star) => Some(BinaryOperator::Multiply),
            Some(Token::Slash) => Some(BinaryOperator::Divide),
            Some(Token::StarStar) => Some(BinaryOperator::Exponent),
            Some(Token::Percent) => Some(BinaryOperator::Modulo),
            Some(Token::And) => Some(BinaryOperator::And),
            Some(Token::Or) => Some(BinaryOperator::Or),
            Some(Token::Less) => Some(BinaryOperator::LessThan),
            Some(Token::LessEqual) => Some(BinaryOperator::LessThanEqual),
            Some(Token::Greater) => Some(BinaryOperator::GreaterThan),
            Some(Token::GreaterEqual) => Some(BinaryOperator::GreaterThanEqual),
            Some(Token::LessGreater) | Some(Token::BangEqual) | Some(Token::Hash) => {
                Some(BinaryOperator::NotEqual)
            }
            _ => None,
        };

        if res.is_some() {
            self.take_token()?;
        }

        Ok(res)
    }

    fn parse_lhs(&mut self) -> anyhow::Result<Exp> {
        let exp = match self.peek_token() {
            Some(Token::Identifier(name)) => {
                self.take_token()?; // consumes identifier
                if self.peek_token() == Some(Token::ColonEqual) {
                    self.parse_assignment(name)?
                } else if self.peek_token() == Some(Token::OpenParens) {
                    self.parse_fun_call(name)?
                } else {
                    Exp::Var(name)
                }
            }
            Some(Token::Int(n)) => {
                self.take_token()?; // consumes int
                Exp::Constant(n)
            }
            Some(_) => {
                self.parse_unary_prefix()?
                // anyhow::bail!("Expected expression, found {token:?}");
            }
            None => {
                anyhow::bail!("Expected expression, found end of file");
            }
        };

        Ok(exp)
    }

    fn parse_assignment(&mut self, name: String) -> anyhow::Result<Exp> {
        let var = Exp::Var(name);
        self.expect(Token::ColonEqual)?;
        let exp = self.parse_exp()?;

        Ok(Exp::Assignment(Box::new(var), Box::new(exp)))
    }

    fn parse_fun_call(&mut self, name: String) -> anyhow::Result<Exp> {
        self.expect(Token::OpenParens)?;

        let mut args = Vec::new();
        loop {
            args.push(self.parse_exp()?);

            if self.peek_token() == Some(Token::CloseParens) {
                break;
            }
        }
        self.expect(Token::CloseParens)?;

        Ok(Exp::FunCall(name, args))
    }

    fn parse_unary_prefix(&mut self) -> anyhow::Result<Exp> {
        let operator = match self.next_token()? {
            Some(Token::Not) => UnaryOperator::Not,
            Some(Token::And) => UnaryOperator::And,
            Some(Token::Plus) => UnaryOperator::Positive,
            Some(Token::Minus) => UnaryOperator::Negative,
            Some(Token::PlusPlus) => UnaryOperator::Increment,
            Some(Token::MinusMinus) => UnaryOperator::Decrement,
            Some(Token::At) => UnaryOperator::Ref,
            Some(token) => {
                return Err(anyhow::anyhow!(
                    "Expected unary operator, found {:?}",
                    token
                ));
            }
            None => {
                return Err(anyhow::anyhow!(
                    "Expected unary operator, found end of file"
                ));
            }
        };

        println!("creating unary with {operator:?}");
        Ok(Exp::Unary(operator, Box::new(self.parse_exp()?)))
    }

    fn expect(&mut self, expected: Token) -> anyhow::Result<()> {
        let Some(actual) = self.next_token()? else {
            anyhow::bail!("Expected {expected:?}, found end of file");
        };

        if actual != expected {
            anyhow::bail!("Expected {expected:?}, found {actual:?}");
        }

        Ok(())
    }

    fn next_token(&mut self) -> anyhow::Result<Option<Token>> {
        let token = self.peek_token();
        self.take_token()?;
        Ok(token)
    }

    fn take_token(&mut self) -> anyhow::Result<()> {
        if self.tokens.is_empty() {
            anyhow::bail!("Premature end of file");
        }

        self.tokens = &self.tokens[1..];
        Ok(())
    }

    fn peek_token(&self) -> Option<Token> {
        self.tokens.first().cloned()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Program {
        Self { statements }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Exp),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Var(String),
    Constant(i32),
    Assignment(Box<Exp>, Box<Exp>),
    FunCall(String, Vec<Exp>),
    Unary(UnaryOperator, Box<Exp>),
    Binary(Box<Exp>, BinaryOperator, Box<Exp>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not,
    And,
    Positive,
    Negative,
    Increment,
    Decrement,
    Ref,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponent,
    Modulo,

    And,
    Or,

    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,

    // TODO: implement those
    SimpleAssign,
    CompoundAssign,
    SubstringComparison,
    AliasAssignment,
    Send,
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn binary_subtraction() {
        let program = r#"nVar2 := 10 - nVar1"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().unwrap();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(Exp::Assignment(
                    Box::new(Exp::Var("nVar2".into())),
                    Box::new(Exp::Binary(
                        Box::new(Exp::Constant(10)),
                        BinaryOperator::Subtract,
                        Box::new(Exp::Var("nVar1".into()),)
                    ))
                ))]
            }
        );
    }

    #[test]
    fn binary_add() {
        let program = r#"2 + 3"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().unwrap();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(Exp::Binary(
                    Box::new(Exp::Constant(2)),
                    BinaryOperator::Add,
                    Box::new(Exp::Constant(3)),
                ))]
            }
        );
    }

    #[test]
    fn unary_not() {
        let program = r#".NOT. a"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().unwrap();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(Exp::Unary(
                    UnaryOperator::Not,
                    Box::new(Exp::Var("a".into()))
                ))]
            }
        );
    }
}
