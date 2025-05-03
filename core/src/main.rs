use std::{fmt, fs, path::PathBuf, process::Command};

use clap::Parser as _;

use lexer::Lexer;
use parser::Parser;

mod assembler;
mod ir;
mod lexer;
mod parser;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Architecture {
    X86_64,
    ARM64,
}

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
    
    /// Target architecture (x86_64 or arm64)
    #[clap(short, long, default_value = "native")]
    target: String,
}

// Get the target architecture based on command line args or system detection
fn determine_target_architecture(requested_target: &str) -> anyhow::Result<Architecture> {
    let arch = match requested_target {
        "x86_64" => Architecture::X86_64,
        "arm64" => Architecture::ARM64,
        "native" => {
            // Auto-detect current architecture
            let output = Command::new("uname")
                .arg("-m")
                .output()?;
            
            let arch_str = String::from_utf8_lossy(&output.stdout).trim().to_string();
            match arch_str.as_str() {
                "x86_64" => Architecture::X86_64,
                "arm64" => Architecture::ARM64,
                unsupported => return Err(anyhow::anyhow!("Unsupported architecture: {}", unsupported)),
            }
        },
        unsupported => return Err(anyhow::anyhow!("Unsupported target architecture: {}", unsupported)),
    };
    
    Ok(arch)
}

fn main() -> anyhow::Result<()> {
    let cli = Args::parse();
    let program = fs::read_to_string(&cli.source)?;

    // Determine target architecture
    let target_arch = determine_target_architecture(&cli.target)?;
    println!("Target architecture: {:?}", target_arch);

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

    let assembler = assembler::Assembler::new(program, target_arch);
    let program = assembler.assemble()?;

    println!("Assembly:");
    for instruction in program.instructions.iter() {
        println!("{instruction:?}");
    }
    println!();

    println!("Code:");
    println!("{program}");
    println!();

    compile(&cli, &program, target_arch)?;

    Ok(())
}

fn compile(args: &Args, program: &assembler::Program, target_arch: Architecture) -> anyhow::Result<()> {
    let file_name = args.source.with_extension("s");
    let executable = &file_name.with_extension("");

    // write the asm file
    fs::write(&file_name, format!("{}", program))?;

    // Determine the compiler to use based on target architecture
    let compiler = match target_arch {
        Architecture::X86_64 => "gcc",
        Architecture::ARM64 => "clang", // macOS ARM64 typically uses clang
    };

    // Create the compiler command
    let mut cmd = Command::new(compiler);
    cmd.arg(&file_name)
       .arg("-o")
       .arg(executable);
    
    // Instead of using the Rust standard library, use a simple C implementation
    // Compile the stdlib.c file for the specific architecture
    let object_file = match target_arch {
        Architecture::X86_64 => "./stdlib_x86_64.o",
        Architecture::ARM64 => "./stdlib_arm64.o",
    };
    
    // Build with the appropriate architecture
    let mut compile_cmd = Command::new("gcc");
    compile_cmd.arg("-c")
              .arg("./stdlib.c")
              .arg("-o")
              .arg(object_file);
              
    // Add architecture flags if cross-compiling
    match target_arch {
        Architecture::X86_64 => {
            if cfg!(target_arch = "aarch64") {
                compile_cmd.arg("-arch").arg("x86_64");
            }
        },
        Architecture::ARM64 => {
            if cfg!(target_arch = "x86_64") {
                compile_cmd.arg("-arch").arg("arm64");
            }
        }
    }
    
    // Compile the stdlib
    let compile_status = compile_cmd.status()?;
    if !compile_status.success() {
        panic!("Failed to compile stdlib.c for {:?}", target_arch);
    }
    
    // Add the compiled object file to the linking
    cmd.arg(object_file);
    
    // Set architecture if cross-compiling
    match target_arch {
        Architecture::X86_64 => {
            if cfg!(target_arch = "aarch64") {
                // Cross-compile to x86_64 if we're on ARM
                cmd.arg("-arch").arg("x86_64");
            }
        },
        Architecture::ARM64 => {
            if cfg!(target_arch = "x86_64") {
                // Cross-compile to ARM64 if we're on x86_64
                cmd.arg("-arch").arg("arm64");
            }
        }
    }
    
    // Run the compiler
    let exit_status = cmd.status()?;

    if !exit_status.success() {
        panic!("Error linking: ${exit_status:?}");
    }

    // remove the temporary asm file
    fs::remove_file(file_name)?;

    println!(
        "Compiled {} to {} for {:?}",
        &args.source.as_path().to_str().unwrap(),
        &executable.as_path().to_str().unwrap(),
        target_arch
    );

    Ok(())
}
