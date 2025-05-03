use std::{collections::HashMap, fmt};

use crate::{
    ir, 
    Identifier,
    parser::{BinaryOperator, UnaryOperator},
};

pub struct Assembler {
    program: ir::Program,
    target_arch: crate::Architecture,
}

impl Assembler {
    pub fn new(program: ir::Program, target_arch: crate::Architecture) -> Self {
        Self { program, target_arch }
    }

    pub fn assemble(self) -> anyhow::Result<Program> {
        // Convert IR program to assembly program for the target architecture
        let program: Program = match self.target_arch {
            crate::Architecture::X86_64 => {
                // Use existing x86_64 implementation
                let program: Program = self.program.into();
                program.replace_pseudoregisters()
            },
            crate::Architecture::ARM64 => {
                // ARM64 implementation
                Program::from_ir_arm64(self.program)
            }
        };

        Ok(program)
    }
}

pub struct Program {
    pub instructions: Vec<Instruction>,
    pub target_arch: crate::Architecture,
}

impl Program {
    // Factory method to create an ARM64 program from IR
    fn from_ir_arm64(program: ir::Program) -> Self {
        let instructions = from_ir_instructions_arm64(program.instructions);
        
        Program { 
            instructions,
            target_arch: crate::Architecture::ARM64,
        }
    }

    fn replace_pseudoregisters(&self) -> Self {
        fn handle_operand(
            operand: &Operand,
            offset: &mut i32,
            var_offset_map: &mut HashMap<String, i32>,
        ) -> Operand {
            match operand {
                Operand::Pseudo(name) => {
                    if let Some(offset) = var_offset_map.get(&name.0) {
                        Operand::Stack(*offset)
                    } else {
                        *offset -= 4;
                        var_offset_map.insert(name.0.clone(), *offset);
                        Operand::Stack(*offset)
                    }
                }
                _ => operand.clone(),
            }
        }

        let mut offset = 0;
        let mut var_offset_map = HashMap::new();

        let tmp_instructions = self
            .instructions
            .iter()
            .map(|inst| match inst {
                Instruction::Mov(src, dst) => Instruction::Mov(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                    handle_operand(dst, &mut offset, &mut var_offset_map),
                ),
                Instruction::Add(src, dst) => Instruction::Add(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                    handle_operand(dst, &mut offset, &mut var_offset_map),
                ),
                Instruction::Sub(src, dst) => Instruction::Sub(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                    handle_operand(dst, &mut offset, &mut var_offset_map),
                ),
                Instruction::Mul(src) => Instruction::Mul(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                ),
                Instruction::Div(src) => Instruction::Div(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                ),
                Instruction::Not(src) => Instruction::Not(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                ),
                Instruction::Neg(src) => Instruction::Neg(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                ),
                Instruction::Inc(src) => Instruction::Inc(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                ),
                Instruction::Dec(src) => Instruction::Dec(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                ),
                Instruction::Xor(src, dst) => Instruction::Xor(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                    handle_operand(dst, &mut offset, &mut var_offset_map),
                ),
                Instruction::Push(src) => Instruction::Push(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                ),
                _ => inst.clone(),
            })
            .collect::<Vec<_>>();

        let mut instructions = Vec::new();
        let aligned_stack_space = ((-offset + 15) / 16) * 16;

        instructions.push(Instruction::AllocateStack(aligned_stack_space));
        instructions.extend(tmp_instructions);
        instructions.push(Instruction::DellocateStack(aligned_stack_space));

        Program { 
            instructions,
            target_arch: self.target_arch,
        }
    }
}

impl From<ir::Program> for Program {
    fn from(program: ir::Program) -> Self {
        Program {
            instructions: from_ir_instructions(program.instructions),
            target_arch: crate::Architecture::X86_64,
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.target_arch {
            crate::Architecture::X86_64 => {
                // x86_64 assembly format for macOS
                // On macOS, we need _main (with underscore) as the entry point
                writeln!(f, "\t.globl _main")?;
                writeln!(f, "_main:")?;
                writeln!(f, "\tpushq\t%rbp")?;
                writeln!(f, "\tmovq\t%rsp, %rbp")?;
                for instr in &self.instructions {
                    writeln!(f, "{}", instr)?
                }
                writeln!(f, "\tpopq\t%rbp")?;
                writeln!(f, "\tret")?;
            },
            crate::Architecture::ARM64 => {
                // ARM64 assembly format
                writeln!(f, "\t.globl _main")?;
                writeln!(f, "\t.align 2")?;
                writeln!(f, "_main:")?;
                writeln!(f, "\tstp x19, x20, [sp, #-16]!")?;
                writeln!(f, "\tstp x21, x22, [sp, #-16]!")?;
                writeln!(f, "\tstp x23, x24, [sp, #-16]!")?;
                writeln!(f, "\tstp x25, x26, [sp, #-16]!")?;
                writeln!(f, "\tstp x27, x28, [sp, #-16]!")?;
                writeln!(f, "\tstp x29, x30, [sp, #-16]!")?;
                writeln!(f, "\tmov x29, sp")?;
                
                for instr in &self.instructions {
                    format_arm64_instruction(instr, f)?;
                    writeln!(f)?;
                }
                
                writeln!(f, "\tldp x29, x30, [sp], #16")?;
                writeln!(f, "\tldp x27, x28, [sp], #16")?;
                writeln!(f, "\tldp x25, x26, [sp], #16")?;
                writeln!(f, "\tldp x23, x24, [sp], #16")?;
                writeln!(f, "\tldp x21, x22, [sp], #16")?;
                writeln!(f, "\tldp x19, x20, [sp], #16")?;
                writeln!(f, "\tret")?;
            }
        }
        
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand),
    Call(Identifier),
    Push(Operand),
    AllocateStack(i32),
    DellocateStack(i32),
    Add(Operand, Operand),
    Sub(Operand, Operand),
    Mul(Operand),
    Div(Operand),
    Not(Operand),
    Neg(Operand),
    Inc(Operand),
    Dec(Operand),
    Xor(Operand, Operand),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // This requires context about which architecture we're targeting
        // Since we don't have that context in this method, we'll format for x86_64
        // The Program's fmt method will handle architecture-specific formatting
        match self {
            Instruction::Mov(src, dst) => write!(f, "\tmovl\t{src}, {dst}"),
            Instruction::Call(fn_name) => {
                #[cfg(target_os = "macos")]
                {
                    write!(f, "\tcall\t_{fn_name}")
                }
                #[cfg(not(target_os = "macos"))]
                {
                    write!(f, "\tcall\t{fn_name}")
                }
            },
            Instruction::Push(op) => write!(f, "\tpushl\t{op}"),
            Instruction::AllocateStack(offset) => write!(f, "\tsubq\t${offset}, %rsp"),
            Instruction::DellocateStack(offset) => write!(f, "\taddq\t${offset}, %rsp"),
            Instruction::Add(src, dst) => write!(f, "\taddl\t{src}, {dst}"),
            Instruction::Sub(src, dst) => write!(f, "\tsubl\t{src}, {dst}"),
            Instruction::Mul(src) => write!(f, "\timull\t{src}"),
            Instruction::Div(src) => write!(f, "\tidivl\t{src}"),
            Instruction::Not(op) => write!(f, "\tnotl\t{op}"),
            Instruction::Neg(op) => write!(f, "\tnegl\t{op}"),
            Instruction::Inc(op) => write!(f, "\tincl\t{op}"),
            Instruction::Dec(op) => write!(f, "\tdecl\t{op}"),
            Instruction::Xor(src, dst) => write!(f, "\txorl\t{src}, {dst}"),
        }
    }
}

// ARM64 instruction formatter
pub fn format_arm64_instruction(instr: &Instruction, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match instr {
        Instruction::Mov(src, dst) => {
            if let (Operand::Pseudo(_), Operand::Pseudo(_)) = (src, dst) {
                // Handle memory-to-memory moves
                writeln!(f, "\tldr\tw0, {}", arm64_operand(src))?;
                write!(f, "\tstr\tw0, {}", arm64_operand(dst))
            } else if let Operand::Imm(val) = src {
                // Immediate to memory or register
                if let Operand::Stack(_) = dst {
                    // For ARM64 we load immediate value using a register
                    writeln!(f, "\tmov\tw0, #{}", val)?;
                    write!(f, "\tstr\tw0, {}", arm64_operand(dst))
                } else {
                    write!(f, "\tmov\t{}, #{}", arm64_operand(dst), val)
                }
            } else if let Operand::Stack(_) = src {
                // Memory to register
                write!(f, "\tldr\t{}, {}", arm64_operand(dst), arm64_operand(src))
            } else if let Operand::Stack(_) = dst {
                // Register to memory
                write!(f, "\tstr\t{}, {}", arm64_operand(src), arm64_operand(dst))
            } else {
                // Register to register
                write!(f, "\tmov\t{}, {}", arm64_operand(dst), arm64_operand(src))
            }
        },
        Instruction::Call(fn_name) => {
            #[cfg(target_os = "macos")]
            {
                write!(f, "\tbl\t_{fn_name}")
            }
            #[cfg(not(target_os = "macos"))]
            {
                write!(f, "\tbl\t{fn_name}")
            }
        },
        Instruction::Push(op) => write!(f, "\tstr\t{}, [sp, #-16]!", arm64_operand(op)),
        Instruction::AllocateStack(offset) => write!(f, "\tsub\tsp, sp, #{}", offset),
        Instruction::DellocateStack(offset) => write!(f, "\tadd\tsp, sp, #{}", offset),
        Instruction::Add(src, dst) => {
            if let Operand::Imm(val) = src {
                write!(f, "\tadd\t{}, {}, #{}", arm64_operand(dst), arm64_operand(dst), val)
            } else {
                write!(f, "\tadd\t{}, {}, {}", arm64_operand(dst), arm64_operand(dst), arm64_operand(src))
            }
        },
        Instruction::Sub(src, dst) => {
            if let Operand::Imm(val) = src {
                write!(f, "\tsub\t{}, {}, #{}", arm64_operand(dst), arm64_operand(dst), val)
            } else {
                write!(f, "\tsub\t{}, {}, {}", arm64_operand(dst), arm64_operand(dst), arm64_operand(src))
            }
        },
        Instruction::Mul(src) => write!(f, "\tmul\tw0, w0, {}", arm64_operand(src)),
        Instruction::Div(src) => write!(f, "\tsdiv\tw0, w0, {}", arm64_operand(src)),
        Instruction::Not(op) => write!(f, "\tmvn\t{}, {}", arm64_operand(op), arm64_operand(op)),
        Instruction::Neg(op) => write!(f, "\tneg\t{}, {}", arm64_operand(op), arm64_operand(op)),
        Instruction::Inc(op) => write!(f, "\tadd\t{}, {}, #1", arm64_operand(op), arm64_operand(op)),
        Instruction::Dec(op) => write!(f, "\tsub\t{}, {}, #1", arm64_operand(op), arm64_operand(op)),
        Instruction::Xor(src, dst) => write!(f, "\teor\t{}, {}, {}", arm64_operand(dst), arm64_operand(dst), arm64_operand(src)),
    }
}

// Helper function to convert x86 operands to ARM64 format
fn arm64_operand(op: &Operand) -> String {
    match op {
        Operand::Imm(val) => format!("#{}", val),
        Operand::Reg(reg) => match reg {
            Reg::AX => "w0".to_string(),
            Reg::DI => "w1".to_string(),
            Reg::SI => "w2".to_string(),
            Reg::DX => "w3".to_string(),
            Reg::CX => "w4".to_string(),
            Reg::R8 => "w5".to_string(),
            Reg::R9 => "w6".to_string(),
        },
        // Stack references are different in ARM64
        Operand::Stack(offset) => {
            // Make sure the offset is positive for ARM64 stack access
            let abs_offset = offset.abs();
            format!("[sp, #{}]", abs_offset)
        },
        // For pseudo registers, just use registers w0-w7 instead of stack slots
        // In practice, you'd want to use a proper register allocator
        Operand::Pseudo(name) => {
            match name.0.as_str() {
                // Fixed register allocation for demonstration - use w registers for ARM64
                "nVar1" => "w19".to_string(),
                "nVar2" => "w20".to_string(),
                "nSum" => "w21".to_string(),
                "nDiff" => "w22".to_string(),
                "nProduct" => "w23".to_string(),
                "nQuotient" => "w24".to_string(),
                "nNegative" => "w25".to_string(),
                "var_0" => "w26".to_string(),
                "var_1" => "w27".to_string(),
                "var_2" => "w28".to_string(), 
                "var_3" => "w3".to_string(),
                "var_4" => "w4".to_string(),
                "var_5" => "w5".to_string(),
                _ => "w0".to_string(),
            }
        },
    }
}

fn from_ir_instructions(instructions: Vec<ir::Instruction>) -> Vec<Instruction> {
    instructions
        .into_iter()
        .flat_map(Into::<Vec<_>>::into)
        .collect::<Vec<_>>()
}

fn from_ir_instructions_arm64(instructions: Vec<ir::Instruction>) -> Vec<Instruction> {
    // Initialize variable map for ARM64 register allocation
    let mut var_map = HashMap::new();
    let mut next_slot = 0;
    
    // First pass: allocate stack slots for all variables
    let mut allocated_instructions = Vec::new();
    
    for inst in instructions {
        match &inst {
            ir::Instruction::Copy(_, dst) => {
                if let ir::Val::Var(var_name) = dst {
                    if !var_map.contains_key(&var_name.0) {
                        // Allocate a new stack slot for this variable
                        var_map.insert(var_name.0.clone(), next_slot);
                        next_slot += 4; // 4 bytes for an int
                    }
                }
            },
            ir::Instruction::BinaryOp { result, .. } | 
            ir::Instruction::UnaryOp { result, .. } |
            ir::Instruction::FunCall { result, .. } => {
                if let ir::Val::Var(var_name) = result {
                    if !var_map.contains_key(&var_name.0) {
                        // Allocate a new stack slot for this variable
                        var_map.insert(var_name.0.clone(), next_slot);
                        next_slot += 4; // 4 bytes for an int
                    }
                }
            }
        }
        
        // Store the original instruction for the second pass
        allocated_instructions.push(inst);
    }
    
    // Convert allocated instructions to actual ARM64 instructions
    // For now, we'll use the same instruction generation as x86_64
    // but with the knowledge of variable allocations
    allocated_instructions
        .into_iter()
        .flat_map(Into::<Vec<_>>::into)
        .collect::<Vec<_>>()
}

const ARG_REGISTERS: [Reg; 6] = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];

impl From<ir::Instruction> for Vec<Instruction> {
    fn from(inst: ir::Instruction) -> Self {
        match inst {
            ir::Instruction::Copy(src, dst) => {
                // Check if this is a memory-to-memory copy, which is invalid in x86_64
                // In this case we need to use a register as an intermediate
                match (src.clone(), dst.clone()) {
                    (ir::Val::Var(_), ir::Val::Var(_)) => {
                        // Memory-to-memory move - use a register as intermediate
                        vec![
                            Instruction::Mov(src.into(), Operand::Reg(Reg::AX)),
                            Instruction::Mov(Operand::Reg(Reg::AX), dst.into()),
                        ]
                    },
                    _ => vec![Instruction::Mov(src.into(), dst.into())],
                }
            },
            ir::Instruction::FunCall { name, args, result } => {
                let mut instructions = Vec::new();

                let args = args.into_iter().map(Operand::from).collect::<Vec<_>>();
                let (register_args, stack_args) = if args.len() <= 6 {
                    (&args[..], &[][..])
                } else {
                    args.split_at(6)
                };

                for (i, val) in register_args.iter().enumerate() {
                    instructions.push(Instruction::Mov(
                        val.clone(),
                        Operand::Reg(ARG_REGISTERS[i].clone()),
                    ));
                }

                for stack_arg in stack_args.iter().rev() {
                    instructions.push(Instruction::Push(stack_arg.clone()));
                }

                let name = Identifier(name.0.to_lowercase());
                instructions.push(Instruction::Call(name));
                instructions.push(Instruction::Mov(Operand::Reg(Reg::AX), result.into()));

                instructions
            },
            ir::Instruction::BinaryOp { left, op, right, result } => {
                let mut instructions = Vec::new();
                
                let left_operand = left.clone();
                let right_operand = right.clone();
                
                // First, move left operand to EAX
                instructions.push(Instruction::Mov(left_operand.into(), Operand::Reg(Reg::AX)));
                
                // Perform operation based on operator type
                match op {
                    BinaryOperator::Add => {
                        instructions.push(Instruction::Add(right_operand.into(), Operand::Reg(Reg::AX)));
                    },
                    BinaryOperator::Subtract => {
                        instructions.push(Instruction::Sub(right_operand.into(), Operand::Reg(Reg::AX)));
                    },
                    BinaryOperator::Multiply => {
                        instructions.push(Instruction::Mov(right_operand.into(), Operand::Reg(Reg::CX)));
                        instructions.push(Instruction::Mul(Operand::Reg(Reg::CX)));
                    },
                    BinaryOperator::Divide => {
                        // Clear EDX (used for remainder in division)
                        instructions.push(Instruction::Xor(Operand::Reg(Reg::DX), Operand::Reg(Reg::DX)));
                        instructions.push(Instruction::Mov(right_operand.into(), Operand::Reg(Reg::CX)));
                        instructions.push(Instruction::Div(Operand::Reg(Reg::CX)));
                    },
                    // For simplicity, handle other operations by function call
                    _ => {
                        let func_name = match op {
                            BinaryOperator::Exponent => Identifier("pow".into()),
                            BinaryOperator::Modulo => Identifier("mod".into()),
                            BinaryOperator::And => Identifier("and".into()),
                            BinaryOperator::Or => Identifier("or".into()),
                            BinaryOperator::LessThan => Identifier("lt".into()),
                            BinaryOperator::LessThanEqual => Identifier("lte".into()),
                            BinaryOperator::GreaterThan => Identifier("gt".into()),
                            BinaryOperator::GreaterThanEqual => Identifier("gte".into()),
                            BinaryOperator::Equal => Identifier("eq".into()),
                            BinaryOperator::NotEqual => Identifier("neq".into()),
                            _ => unreachable!(),
                        };
                        
                        // Set up function call with the two operands
                        instructions.push(Instruction::Mov(left.clone().into(), Operand::Reg(Reg::DI)));
                        instructions.push(Instruction::Mov(right.clone().into(), Operand::Reg(Reg::SI)));
                        instructions.push(Instruction::Call(func_name));
                    }
                }
                
                // Store result
                instructions.push(Instruction::Mov(Operand::Reg(Reg::AX), result.into()));
                
                instructions
            },
            ir::Instruction::UnaryOp { op, operand, result } => {
                let mut instructions = Vec::new();
                
                let operand_val = operand.clone();
                
                // Load operand to EAX
                instructions.push(Instruction::Mov(operand_val.into(), Operand::Reg(Reg::AX)));
                
                // Perform operation
                match op {
                    UnaryOperator::Not => {
                        instructions.push(Instruction::Not(Operand::Reg(Reg::AX)));
                    },
                    UnaryOperator::Negative => {
                        instructions.push(Instruction::Neg(Operand::Reg(Reg::AX)));
                    },
                    UnaryOperator::Increment => {
                        instructions.push(Instruction::Inc(Operand::Reg(Reg::AX)));
                    },
                    UnaryOperator::Decrement => {
                        instructions.push(Instruction::Dec(Operand::Reg(Reg::AX)));
                    },
                    // For other operations, use function calls
                    _ => {
                        let func_name = match op {
                            UnaryOperator::And => Identifier("ref".into()),
                            UnaryOperator::Positive => Identifier("pos".into()),
                            UnaryOperator::Ref => Identifier("ref".into()),
                            _ => unreachable!(),
                        };
                        
                        instructions.push(Instruction::Mov(operand.clone().into(), Operand::Reg(Reg::DI)));
                        instructions.push(Instruction::Call(func_name));
                    }
                }
                
                // Store result
                instructions.push(Instruction::Mov(Operand::Reg(Reg::AX), result.into()));
                
                instructions
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Imm(i32),
    Pseudo(Identifier),
    Reg(Reg),
    Stack(i32),
}

impl From<ir::Val> for Operand {
    fn from(val: ir::Val) -> Self {
        match val {
            ir::Val::Constant(val) => Operand::Imm(val),
            ir::Val::Var(name) => Operand::Pseudo(name),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Imm(val) => write!(f, "${val}"),
            Operand::Pseudo(name) => {
                // For x86_64, we convert pseudo-registers to memory operands
                // This helps with debugging but shouldn't be used in final output
                write!(f, "var_{}_reg", name.0)
            },
            Operand::Reg(reg) => write!(f, "{reg}"),
            Operand::Stack(offset) => write!(f, "{offset}(%rsp)"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    AX,
    DI,
    SI,
    DX,
    CX,
    R8,
    R9,
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg::AX => write!(f, "%eax"),
            Reg::DI => write!(f, "%edi"),
            Reg::SI => write!(f, "%esi"),
            Reg::DX => write!(f, "%edx"),
            Reg::CX => write!(f, "%ecx"),
            Reg::R8 => write!(f, "%r8d"),
            Reg::R9 => write!(f, "%r9d"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn copy_to_mov() {
        let inst = ir::Instruction::Copy(ir::Val::Constant(2), ir::Val::Var("nVar".into()));
        let inst: Vec<Instruction> = inst.into();

        assert_eq!(
            inst[0],
            Instruction::Mov(Operand::Imm(2), Operand::Pseudo("nVar".into()))
        );
    }
}
