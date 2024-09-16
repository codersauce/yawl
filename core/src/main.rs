use std::{fmt, fs, path::PathBuf, process::Command};

use clap::Parser as _;

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

#[derive(clap::Parser)]
struct Args {
    /// Source file to compile
    source: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Args::parse();
    let program = fs::read_to_string(&cli.source)?;

    let mut lexer = Lexer::new(&program);
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

    compile(&cli, &program)?;

    Ok(())
}

fn compile(args: &Args, program: &assembler::Program) -> anyhow::Result<()> {
    let file_name = args.source.with_extension("s");
    let executable = &file_name.with_extension("");

    // write the asm file
    fs::write(&file_name, format!("{}", program))?;

    // link it
    let exit_status = Command::new("gcc")
        .arg(&file_name)
        .arg("-o")
        .arg(executable)
        .arg("-L./target/debug")
        .arg("-l")
        .arg("yawl_stdlib")
        .status()?;

    if !exit_status.success() {
        panic!("Error linking: ${exit_status:?}");
    }

    // remove the temporary asm file
    fs::remove_file(file_name)?;

    println!(
        "Compiled {} to {}",
        &args.source.as_path().to_str().unwrap(),
        &executable.as_path().to_str().unwrap()
    );

    Ok(())
}
