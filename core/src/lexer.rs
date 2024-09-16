use regex::Regex;
use strum::{EnumProperty, IntoEnumIterator};
use strum_macros::{EnumIter, EnumProperty};

pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Self { source, pos: 0 }
    }

    pub fn tokenize(&mut self) -> anyhow::Result<Vec<Token>> {
        let mut tokens = Vec::new();

        while self.pos < self.source.len() {
            let mut found = false;

            self.skip_whitespaces();

            if self.pos >= self.source.len() {
                break;
            }

            if self.skip_comments() {
                continue;
            }

            for token in Token::iter() {
                if let Some(re) = token.regex() {
                    let re = Regex::new(re)?;
                    if let Some(capts) = re.captures(&self.source[self.pos..]) {
                        let matched = capts.get(0).unwrap().as_str();
                        let token = match token {
                            Token::Identifier(_) => Token::Identifier(matched.to_string()),
                            Token::Int(_) => Token::Int(matched.parse().unwrap()),
                            t => t,
                        };
                        tokens.push(token);
                        self.pos += matched.len();
                        found = true;
                    }
                }
            }

            if !found {
                anyhow::bail!("Unexpected token: {:?}", &self.source[self.pos..]);
            }
        }

        Ok(tokens)
    }

    fn skip_whitespaces(&mut self) {
        while let Some(ch) = self.source.chars().nth(self.pos) {
            if !ch.is_whitespace() {
                break;
            }
            self.pos += 1;
        }
    }

    fn skip_comments(&mut self) -> bool {
        if let Some(ch) = self.source.chars().nth(self.pos) {
            if ch == '/' {
                self.pos += 1;
                if let Some(ch) = self.source.chars().nth(self.pos) {
                    if ch == '/' {
                        self.pos += 2;
                        // eat everything until \n
                        while let Some(ch) = self.source.chars().nth(self.pos) {
                            if ch == '\n' {
                                return true;
                            }
                            self.pos += 1;
                        }
                    }
                }
            }
        }

        false
    }
}

#[derive(Debug, Clone, PartialEq, EnumProperty, EnumIter)]
pub enum Token {
    #[strum(props(regex = r"^[a-zA-Z_][a-zA-Z0-9_]*"))]
    Identifier(String),
    #[strum(props(regex = r"^[0-9]+"))]
    Int(i32),
    #[strum(props(regex = r"^:="))]
    ColonEqual,
    #[strum(props(regex = r"^\("))]
    OpenParens,
    #[strum(props(regex = r"^\)"))]
    CloseParens,
}

impl Token {
    pub fn regex(&self) -> Option<&'static str> {
        self.get_str("regex")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn double_slash() {
        let program = r#"
        // comment
        nVar := 1 // inline comment
        "#;

        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("nVar".to_string()),
                Token::ColonEqual,
                Token::Int(1)
            ]
        );
    }
}
