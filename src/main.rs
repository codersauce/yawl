use std::fmt;

use lexer::Lexer;
use parser::Parser;

mod assembler;
mod ir;
mod lexer;
mod parser;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(String);

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Identifier(value)
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier(value.to_string())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn main() -> anyhow::Result<()> {
    let program = r#"
    nVar1 := 2
    ERRORLEVEL(nVar1)
    "#;

    let mut lexer = Lexer::new(program);
    let tokens = lexer.tokenize()?;

    println!("Tokens:");
    for token in tokens.iter() {
        println!("{token:?}");
    }
    println!();

    let mut parser = Parser::new(&tokens);
    let program = parser.parse()?;

    println!("AST:");
    for st in program.statements.iter() {
        println!("{st:?}");
    }
    println!();

    let mut ir_generator = ir::IrGenerator::new(program);
    let program = ir_generator.generate()?;

    println!("IR:");
    for instruction in program.instructions.iter() {
        println!("{instruction:?}");
    }
    println!();

    let assembler = assembler::Assembler::new(program);
    let program = assembler.assemble()?;

    println!("Assembly:");
    for instruction in program.instructions.iter() {
        println!("{instruction:?}");
    }
    println!();

    println!("Code:");
    println!("{program}");
    println!();

    fs::write("test.s", format!("{}", program))?;

    Ok(())
}
