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
        let exp = match self.next_token()? {
            Some(Token::Identifier(name)) => {
                if self.peek_token() == Some(Token::ColonEqual) {
                    self.parse_assignment(name)?
                } else if self.peek_token() == Some(Token::OpenParens) {
                    self.parse_fun_call(name)?
                } else {
                    Exp::Var(name)
                }
            }
            Some(Token::Int(n)) => Exp::Constant(n),
            Some(token) => {
                anyhow::bail!("Expected expression, found {token:?}");
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

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Program {
        Self { statements }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Exp),
}

#[derive(Debug, Clone)]
pub enum Exp {
    Var(String),
    Constant(i32),
    Assignment(Box<Exp>, Box<Exp>),
    FunCall(String, Vec<Exp>),
}
