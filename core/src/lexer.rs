use regex::Regex;
use strum::{EnumProperty, IntoEnumIterator};
use strum_macros::{EnumIter, EnumProperty};

use crate::Span;

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

        let mut all_tokens = TokenKind::iter().collect::<Vec<_>>();
        all_tokens.sort_by_key(|t| t.len());

        while self.pos < self.source.len() {
            let mut found = false;

            self.skip_whitespaces();

            if self.pos >= self.source.len() {
                break;
            }

            if self.skip_comments() {
                continue;
            }

            for token in all_tokens.iter().rev() {
                if let Some(re) = token.regex() {
                    let re = Regex::new(re)?;
                    if let Some(capts) = re.captures(&self.source[self.pos..]) {
                        let matched = capts.get(0).unwrap().as_str();
                        let token = match token {
                            TokenKind::Identifier(_) => TokenKind::Identifier(matched.to_string()),
                            TokenKind::Int(_) => TokenKind::Int(matched.parse().unwrap()),
                            t => t.clone(),
                        };
                        tokens.push(Token {
                            kind: token,
                            origin: matched,
                            span: Span {
                                start: self.pos,
                                end: self.pos + matched.len(),
                            },
                        });
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
                if let Some(ch) = self.source.chars().nth(self.pos + 1) {
                    if ch == '/' {
                        self.pos += 3;
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

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub origin: &'a str,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, EnumProperty, EnumIter)]
pub enum TokenKind {
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
    #[strum(props(regex = r"^\+"))]
    Plus,
    #[strum(props(regex = r"^-"))]
    Minus,
    #[strum(props(regex = r"^\*"))]
    Star,
    #[strum(props(regex = r"^/"))]
    Slash,
    #[strum(props(regex = r"^="))]
    Equal,
    #[strum(props(regex = r"^\+="))]
    PlusEqual,
    #[strum(props(regex = r"^-\="))]
    MinusEqual,
    #[strum(props(regex = r"^\*="))]
    StarEqual,
    #[strum(props(regex = r"^/="))]
    SlashEqual,
    #[strum(props(regex = r"^%="))]
    PercentEqual,
    #[strum(props(regex = r"^\^="))]
    CaretEqual,
    #[strum(props(regex = r"^=="))]
    EqualEqual,
    #[strum(props(regex = r"^!="))]
    BangEqual,
    #[strum(props(regex = r"^>"))]
    Greater,
    #[strum(props(regex = r"^>="))]
    GreaterEqual,
    #[strum(props(regex = r"^<"))]
    Less,
    #[strum(props(regex = r"^<="))]
    LessEqual,
    #[strum(props(regex = r"^<>"))]
    LessGreater,
    #[strum(props(regex = r"^#"))]
    Hash,
    #[strum(props(regex = r"^\.OR\."))]
    Or,
    #[strum(props(regex = r"^\.AND\."))]
    And,
    #[strum(props(regex = r"^\.NOT\."))]
    Not,
    #[strum(props(regex = r"^\*\*"))]
    StarStar,
    #[strum(props(regex = r"^\^"))]
    Caret,
    #[strum(props(regex = r"^%"))]
    Percent,
    #[strum(props(regex = r"^->"))]
    Arrow,
    #[strum(props(regex = r"^\$"))]
    Dollar,
    #[strum(props(regex = r"^&"))]
    Ampersand,
    #[strum(props(regex = r"^\+\+"))]
    PlusPlus,
    #[strum(props(regex = r"^--"))]
    MinusMinus,
    #[strum(props(regex = r"^@"))]
    At,
    #[strum(props(regex = r"^:"))]
    Colon,
}

impl TokenKind {
    pub fn len(&self) -> usize {
        self.regex().unwrap_or("").len()
    }

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

        println!("{:?}", tokens);

        // assert_eq!(
        //     tokens,
        //     vec![
        //         TokenKind::Identifier("nVar".to_string()),
        //         TokenKind::ColonEqual,
        //         TokenKind::Int(1)
        //     ]
        // );
    }
    //
    // #[test]
    // fn test_plus() {
    //     let program = r#"x := 1 + 1"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("x".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Int(1),
    //             TokenKind::Plus,
    //             TokenKind::Int(1),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_star() {
    //     let program = r#"y := 2 * 2"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("y".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Int(2),
    //             TokenKind::Star,
    //             TokenKind::Int(2),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_slash() {
    //     let program = r#"z := z / y"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("z".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("z".to_string()),
    //             TokenKind::Slash,
    //             TokenKind::Identifier("y".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_minus() {
    //     let program = r#"a := z - y"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("z".to_string()),
    //             TokenKind::Minus,
    //             TokenKind::Identifier("y".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_equal_equal() {
    //     let program = r#"b := a == z"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("b".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::EqualEqual,
    //             TokenKind::Identifier("z".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_greater() {
    //     let program = r#"c := a > b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("c".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::Greater,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_less() {
    //     let program = r#"d := a < b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("d".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::Less,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_greater_equal() {
    //     let program = r#"e := a >= b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("e".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::GreaterEqual,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_less_equal() {
    //     let program = r#"f := a <= b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("f".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::LessEqual,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_less_greater() {
    //     let program = r#"g := a <> b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("g".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::LessGreater,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_bang_equal() {
    //     let program = r#"h := a != b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("h".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::BangEqual,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_hash() {
    //     let program = r#"j := a # b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("j".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::Hash,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_or() {
    //     let program = r#"k := a .OR. b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("k".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::Or,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_and() {
    //     let program = r#"l := a .AND. b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("l".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::And,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_star_star() {
    //     let program = r#"m := a ** b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("m".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::StarStar,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_caret() {
    //     let program = r#"m := a ^ b"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("m".to_string()),
    //             TokenKind::ColonEqual,
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::Caret,
    //             TokenKind::Identifier("b".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_arrow() {
    //     let program = r#"OBJ->FIELD"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("OBJ".to_string()),
    //             TokenKind::Arrow,
    //             TokenKind::Identifier("FIELD".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_dollar() {
    //     let program = r#"x $ y"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("x".to_string()),
    //             TokenKind::Dollar,
    //             TokenKind::Identifier("y".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_colon() {
    //     let program = r#"myBrowse:pageUp()"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("myBrowse".to_string()),
    //             TokenKind::Colon,
    //             TokenKind::Identifier("pageUp".to_string()),
    //             TokenKind::OpenParens,
    //             TokenKind::CloseParens,
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_compound_plus() {
    //     let program = "a += 1";
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::PlusEqual,
    //             TokenKind::Int(1),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_compound_minus() {
    //     let program = "a -= 1";
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::MinusEqual,
    //             TokenKind::Int(1),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_compound_star() {
    //     let program = "a *= 1";
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::StarEqual,
    //             TokenKind::Int(1),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_compound_slash() {
    //     let program = "a /= 1";
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::SlashEqual,
    //             TokenKind::Int(1),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_compound_percent() {
    //     let program = "a %= 1";
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::PercentEqual,
    //             TokenKind::Int(1),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_compound_caret() {
    //     let program = "a ^= 1";
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::Identifier("a".to_string()),
    //             TokenKind::CaretEqual,
    //             TokenKind::Int(1),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_minus_minus() {
    //     let program = r#"--a"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![
    //             TokenKind::MinusMinus,
    //             TokenKind::Identifier("a".to_string()),
    //         ]
    //     );
    // }
    //
    // #[test]
    // fn test_not() {
    //     let program = r#".NOT. a"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![TokenKind::Not, TokenKind::Identifier("a".to_string()),]
    //     );
    // }
    //
    // #[test]
    // fn test_at() {
    //     let program = r#"@a"#;
    //     let mut lexer = Lexer::new(program);
    //     let tokens = lexer.tokenize().unwrap();
    //     assert_eq!(
    //         tokens,
    //         vec![TokenKind::At, TokenKind::Identifier("a".to_string()),]
    //     );
    // }
}
