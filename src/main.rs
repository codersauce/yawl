use lexer::Lexer;
use parser::Parser;

mod ir;
mod lexer;
mod parser;

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
    let instructions = ir_generator.generate()?;

    println!("IR:");
    for instruction in instructions {
        println!("{instruction:?}");
    }

    Ok(())
}
