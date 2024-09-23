use std::{fmt, fs, path::PathBuf, process::Command};

use assembler::to_code;
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn empty() -> Span {
        Span { start: 0, end: 0 }
    }

    pub fn join(&self, other: &Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }

    pub fn value(&self, source: &str) -> String {
        source[self.start..self.end].to_string()
    }
}

#[derive(clap::Parser)]
struct Args {
    /// Source file to compile
    source: PathBuf,

    /// Keep the temporary assembly file
    #[clap(short, long)]
    keep_asm: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = Args::parse();
    let source = fs::read_to_string(&cli.source)?;

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize()?;

    println!("Tokens:");
    for token in tokens.iter() {
        println!("{token:?}");
    }
    println!();

    let mut parser = Parser::new(&source, &tokens);
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
    println!("{}", to_code(&program, &source));
    println!();

    compile(&cli, &source, &program)?;

    Ok(())
}

fn compile(args: &Args, source: &str, program: &assembler::Program) -> anyhow::Result<()> {
    let file_name = args.source.with_extension("s");
    let executable = &file_name.with_extension("");

    // write the asm file
    fs::write(&file_name, format!("{}", to_code(program, source)))?;

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
    if !args.keep_asm {
        fs::remove_file(file_name)?;
    }

    println!(
        "Compiled {} to {}",
        &args.source.as_path().to_str().unwrap(),
        &executable.as_path().to_str().unwrap()
    );

    Ok(())
}
