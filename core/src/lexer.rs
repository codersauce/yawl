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

        let mut all_tokens = Token::iter().collect::<Vec<_>>();
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
                            Token::Identifier(_) => Token::Identifier(matched.to_string()),
                            Token::Int(_) => Token::Int(matched.parse().unwrap()),
                            t => t.clone(),
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

impl Token {
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

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("nVar".to_string()),
                Token::ColonEqual,
                Token::Int(1)
            ]
        );
    }

    #[test]
    fn test_plus() {
        let program = r#"x := 1 + 1"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::ColonEqual,
                Token::Int(1),
                Token::Plus,
                Token::Int(1),
            ]
        );
    }

    #[test]
    fn test_star() {
        let program = r#"y := 2 * 2"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("y".to_string()),
                Token::ColonEqual,
                Token::Int(2),
                Token::Star,
                Token::Int(2),
            ]
        );
    }

    #[test]
    fn test_slash() {
        let program = r#"z := z / y"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("z".to_string()),
                Token::ColonEqual,
                Token::Identifier("z".to_string()),
                Token::Slash,
                Token::Identifier("y".to_string()),
            ]
        );
    }

    #[test]
    fn test_minus() {
        let program = r#"a := z - y"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("a".to_string()),
                Token::ColonEqual,
                Token::Identifier("z".to_string()),
                Token::Minus,
                Token::Identifier("y".to_string()),
            ]
        );
    }

    #[test]
    fn test_equal_equal() {
        let program = r#"b := a == z"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("b".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::EqualEqual,
                Token::Identifier("z".to_string()),
            ]
        );
    }

    #[test]
    fn test_greater() {
        let program = r#"c := a > b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("c".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::Greater,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_less() {
        let program = r#"d := a < b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("d".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::Less,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_greater_equal() {
        let program = r#"e := a >= b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("e".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::GreaterEqual,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_less_equal() {
        let program = r#"f := a <= b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("f".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::LessEqual,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_less_greater() {
        let program = r#"g := a <> b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("g".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::LessGreater,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_bang_equal() {
        let program = r#"h := a != b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("h".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::BangEqual,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_hash() {
        let program = r#"j := a # b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("j".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::Hash,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_or() {
        let program = r#"k := a .OR. b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("k".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::Or,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_and() {
        let program = r#"l := a .AND. b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("l".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::And,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_star_star() {
        let program = r#"m := a ** b"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("m".to_string()),
                Token::ColonEqual,
                Token::Identifier("a".to_string()),
                Token::StarStar,
                Token::Identifier("b".to_string()),
            ]
        );
    }

    #[test]
    fn test_arrow() {
        let program = r#"OBJ->FIELD"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("OBJ".to_string()),
                Token::Arrow,
                Token::Identifier("FIELD".to_string()),
            ]
        );
    }

    #[test]
    fn test_dollar() {
        let program = r#"x $ y"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Dollar,
                Token::Identifier("y".to_string()),
            ]
        );
    }

    #[test]
    fn test_colon() {
        let program = r#"myBrowse:pageUp()"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("myBrowse".to_string()),
                Token::Colon,
                Token::Identifier("pageUp".to_string()),
                Token::OpenParens,
                Token::CloseParens,
            ]
        );
    }

    #[test]
    fn test_minus_minus() {
        let program = r#"--a"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::MinusMinus, Token::Identifier("a".to_string()),]
        );
    }

    #[test]
    fn test_not() {
        let program = r#".NOT. a"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::Not, Token::Identifier("a".to_string()),]
        );
    }

    #[test]
    fn test_at() {
        let program = r#"@a"#;
        let mut lexer = Lexer::new(program);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::At, Token::Identifier("a".to_string()),]);
    }
}
