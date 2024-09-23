use crate::{
    lexer::{Token, TokenKind},
    Span,
};

#[allow(unused)]
pub struct Parser<'a> {
    source: &'a str,
    tokens: &'a [Token<'a>],
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: &'a [Token]) -> Parser<'a> {
        Self { source, tokens }
    }

    pub fn parse(&mut self) -> anyhow::Result<Program> {
        let mut statements = Vec::new();

        while self.peek_token().is_some() {
            statements.push(self.parse_statement()?);
        }

        Ok(Program::new(statements))
    }

    fn parse_statement(&mut self) -> anyhow::Result<Statement> {
        let (exp, span) = self.parse_exp()?;
        Ok(Statement::Expression(exp, span))
    }

    fn parse_exp(&mut self) -> anyhow::Result<(Exp, Span)> {
        let left = self.parse_lhs()?;

        if let Some((op, _)) = self.parse_binary_operator()? {
            let (right, right_span) = self.parse_exp()?;
            let span = left.span().join(&right_span);
            return Ok((Exp::Binary(Box::new(left), op, Box::new(right), span), span));
        }

        let span = left.span();
        Ok((left, span))
    }

    fn parse_binary_operator(&mut self) -> anyhow::Result<Option<(BinaryOperator, Span)>> {
        let res = match self.peek_token().map(|t| t.kind) {
            Some(TokenKind::Plus) => Some(BinaryOperator::Add),
            Some(TokenKind::Minus) => Some(BinaryOperator::Subtract),
            Some(TokenKind::Star) => Some(BinaryOperator::Multiply),
            Some(TokenKind::Slash) => Some(BinaryOperator::Divide),
            Some(TokenKind::StarStar) | Some(TokenKind::Caret) => Some(BinaryOperator::Exponent),
            Some(TokenKind::Percent) => Some(BinaryOperator::Modulo),
            Some(TokenKind::And) => Some(BinaryOperator::And),
            Some(TokenKind::Or) => Some(BinaryOperator::Or),
            Some(TokenKind::Equal) => Some(BinaryOperator::Equal),
            Some(TokenKind::Less) => Some(BinaryOperator::LessThan),
            Some(TokenKind::LessEqual) => Some(BinaryOperator::LessThanEqual),
            Some(TokenKind::Greater) => Some(BinaryOperator::GreaterThan),
            Some(TokenKind::GreaterEqual) => Some(BinaryOperator::GreaterThanEqual),
            Some(TokenKind::LessGreater) | Some(TokenKind::BangEqual) | Some(TokenKind::Hash) => {
                Some(BinaryOperator::NotEqual)
            }
            _ => None,
        };

        if res.is_some() {
            let token = self.take_token()?;
            return Ok(Some((res.unwrap(), token.span)));
        }

        Ok(None)
    }

    fn parse_lhs(&mut self) -> anyhow::Result<Exp> {
        let Some(token) = self.peek_token() else {
            anyhow::bail!("Expected expression, found end of file");
        };

        let kind = token.kind;
        let left_span = token.span;
        let exp = match &kind {
            TokenKind::Identifier(name) => {
                self.take_token()?; // consumes identifier
                let kind = self.peek_token().map(|t| t.kind);

                if self.is_assignment() {
                    self.parse_assignment(name.to_string(), left_span)?
                } else if kind == Some(TokenKind::OpenParens) {
                    self.parse_fun_call(name.to_string(), left_span)?
                } else {
                    Exp::Var(name.to_string(), left_span)
                }
            }
            TokenKind::Int(n) => {
                let token = self.take_token()?; // consumes int
                Exp::Constant(*n, token.span)
            }
            _ => {
                self.parse_unary_prefix()?
                // anyhow::bail!("Expected expression, found {token:?}");
            }
        };

        Ok(exp)
    }

    fn is_assignment(&self) -> bool {
        let token = self.peek_token().map(|t| t.kind);
        token == Some(TokenKind::ColonEqual)
            || token == Some(TokenKind::PlusEqual)
            || token == Some(TokenKind::MinusEqual)
            || token == Some(TokenKind::StarEqual)
            || token == Some(TokenKind::SlashEqual)
            || token == Some(TokenKind::PercentEqual)
            || token == Some(TokenKind::CaretEqual)
    }

    fn parse_assignment(&mut self, name: String, name_span: Span) -> anyhow::Result<Exp> {
        // TODO: verify spans
        let var = Exp::Var(name, name_span);
        let token = self.take_token()?;
        let kind = token.kind;
        let (exp, span) = self.parse_exp()?;

        let span = var.span().join(&span);
        match kind {
            TokenKind::ColonEqual => Ok(Exp::Assignment(Box::new(var), Box::new(exp), span)),
            TokenKind::PlusEqual => Ok(Exp::Assignment(
                Box::new(var.clone()),
                Box::new(Exp::Binary(
                    Box::new(var),
                    BinaryOperator::Add,
                    Box::new(exp),
                    span,
                )),
                span,
            )),
            TokenKind::MinusEqual => Ok(Exp::Assignment(
                Box::new(var.clone()),
                Box::new(Exp::Binary(
                    Box::new(var),
                    BinaryOperator::Subtract,
                    Box::new(exp),
                    span,
                )),
                span,
            )),
            TokenKind::StarEqual => Ok(Exp::Assignment(
                Box::new(var.clone()),
                Box::new(Exp::Binary(
                    Box::new(var),
                    BinaryOperator::Multiply,
                    Box::new(exp),
                    span,
                )),
                span,
            )),
            TokenKind::SlashEqual => Ok(Exp::Assignment(
                Box::new(var.clone()),
                Box::new(Exp::Binary(
                    Box::new(var),
                    BinaryOperator::Divide,
                    Box::new(exp),
                    span,
                )),
                span,
            )),
            TokenKind::PercentEqual => Ok(Exp::Assignment(
                Box::new(var.clone()),
                Box::new(Exp::Binary(
                    Box::new(var),
                    BinaryOperator::Modulo,
                    Box::new(exp),
                    span,
                )),
                span,
            )),
            TokenKind::CaretEqual => Ok(Exp::Assignment(
                Box::new(var.clone()),
                Box::new(Exp::Binary(
                    Box::new(var),
                    BinaryOperator::Exponent,
                    Box::new(exp),
                    span,
                )),
                span,
            )),
            _ => {
                // TODO: Augment error with span
                anyhow::bail!("Expected assignment operator, found {:?}", kind);
            }
        }
    }

    fn parse_fun_call(&mut self, name: String, name_span: Span) -> anyhow::Result<Exp> {
        self.expect(TokenKind::OpenParens)?;

        let mut args = Vec::new();
        loop {
            let (exp, _) = self.parse_exp()?;
            args.push(exp);

            if self.peek_token().map(|t| t.kind) == Some(TokenKind::CloseParens) {
                break;
            }
        }
        let token = self.expect(TokenKind::CloseParens)?;
        let span = name_span.join(&token.span);

        Ok(Exp::FunCall(name, args, span))
    }

    fn parse_unary_prefix(&mut self) -> anyhow::Result<Exp> {
        let token = self.take_token()?;
        let span = token.span;
        let operator = match token.kind {
            TokenKind::Not => UnaryOperator::Not,
            TokenKind::And => UnaryOperator::And,
            TokenKind::Plus => UnaryOperator::Positive,
            TokenKind::Minus => UnaryOperator::Negative,
            TokenKind::PlusPlus => UnaryOperator::Increment,
            TokenKind::MinusMinus => UnaryOperator::Decrement,
            TokenKind::At => UnaryOperator::Ref,
            token => {
                return Err(anyhow::anyhow!(
                    "Expected unary operator, found {:?}",
                    token
                ));
            }
        };

        let (exp, exp_span) = self.parse_exp()?;
        let span = span.join(&exp_span);
        Ok(Exp::Unary(operator, Box::new(exp), span))
    }

    fn expect(&mut self, expected: TokenKind) -> anyhow::Result<Token> {
        let token = self.take_token()?;
        let actual = token.kind.clone();
        if actual != expected {
            anyhow::bail!("Expected {expected:?}, found {actual:?}");
        }

        Ok(token)
    }

    fn take_token(&mut self) -> anyhow::Result<Token> {
        if self.tokens.is_empty() {
            anyhow::bail!("Premature end of file");
        }

        let ret = self.tokens[0].clone();
        self.tokens = &self.tokens[1..];
        Ok(ret)
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
    Expression(Exp, Span),
}

impl Statement {
    #[allow(unused)]
    pub fn span(&self) -> Span {
        let Statement::Expression(_, span) = self;
        *span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Var(String, Span),
    Constant(i32, Span),
    Assignment(Box<Exp>, Box<Exp>, Span),
    FunCall(String, Vec<Exp>, Span),
    Unary(UnaryOperator, Box<Exp>, Span),
    Binary(Box<Exp>, BinaryOperator, Box<Exp>, Span),
}

impl Exp {
    pub fn span(&self) -> Span {
        match self {
            Exp::Var(_, span) => *span,
            Exp::Constant(_, span) => *span,
            Exp::Assignment(_, _, span) => *span,
            Exp::FunCall(_, _, span) => *span,
            Exp::Unary(_, _, span) => *span,
            Exp::Binary(_, _, _, span) => *span,
        }
    }
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
    // SubstringComparison,
    // AliasAssignment,
    // Send,
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn binary_subtraction() {
        let source = r#"nVar2 := 10 - nVar1"#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(source, &tokens);
        let program = parser.parse().unwrap();

        let st = program.statements.first().unwrap();
        assert_eq!(st.span().value(source), "nVar2 := 10 - nVar1");

        let Statement::Expression(exp, _) = st;
        assert_eq!(exp.span().value(source), "nVar2 := 10 - nVar1");

        let Exp::Assignment(var, exp, _) = exp else {
            panic!("Expected assignment, found {:?}", exp);
        };
        assert_eq!(var.span().value(source), "nVar2");
        assert_eq!(exp.span().value(source), "10 - nVar1");

        let Exp::Binary(left, op, right, _) = exp.as_ref() else {
            panic!("Expected binary, found {:?}", exp);
        };
        assert_eq!(left.span().value(source), "10");
        assert_eq!(op, &BinaryOperator::Subtract);
        assert_eq!(right.span().value(source), "nVar1");
    }

    #[test]
    fn test_operators() {
        let source = r#"
        nVar0 := -1 // -1
        nVar1 := 2 + 2 + nVar0 // 3
        ERRORLEVEL(nVar1)
        "#;

        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(source, &tokens);
        let program = parser.parse().unwrap();

        println!("{:?}", program);
    }

    //     #[test]
    //     fn binary_add() {
    //         let program = r#"2 + 3"#;
    //         let mut lexer = Lexer::new(program);
    //         let tokens = lexer.tokenize().unwrap();
    //         let mut parser = Parser::new(program, &tokens);
    //         let program = parser.parse().unwrap();
    //
    //         assert_eq!(
    //             program,
    //             Program {
    //                 statements: vec![Statement::Expression(Exp::Binary(
    //                     Box::new(Exp::Constant(2)),
    //                     BinaryOperator::Add,
    //                     Box::new(Exp::Constant(3)),
    //                 ))]
    //             }
    //         );
    //     }
    //
    //     #[test]
    //     fn unary_not() {
    //         let program = r#".NOT. a"#;
    //         let mut lexer = Lexer::new(program);
    //         let tokens = lexer.tokenize().unwrap();
    //         let mut parser = Parser::new(program, &tokens);
    //         let program = parser.parse().unwrap();
    //
    //         assert_eq!(
    //             program,
    //             Program {
    //                 statements: vec![Statement::Expression(Exp::Unary(
    //                     UnaryOperator::Not,
    //                     Box::new(Exp::Var("a".into()))
    //                 ))]
    //             }
    //         );
    //     }
    //
    //     #[test]
    //     fn negative() {
    //         let program = "-1";
    //         let mut lexer = Lexer::new(program);
    //         let tokens = lexer.tokenize().unwrap();
    //         let mut parser = Parser::new(program, &tokens);
    //         let program = parser.parse().unwrap();
    //
    //         println!("{:?}", program);
    //     }
}
