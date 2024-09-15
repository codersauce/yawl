use std::{collections::HashMap, fmt};

use crate::{ir, Identifier};

pub struct Assembler {
    program: ir::Program,
}

impl Assembler {
    pub fn new(program: ir::Program) -> Self {
        Self { program }
    }

    pub fn assemble(self) -> anyhow::Result<Program> {
        let program: Program = self.program.into();
        let program = program.replace_pseudoregisters();

        Ok(program)
    }
}

pub struct Program {
    pub instructions: Vec<Instruction>,
}

impl Program {
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

        let instructions = self
            .instructions
            .iter()
            .map(|inst| match inst {
                Instruction::Mov(src, dst) => Instruction::Mov(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                    handle_operand(dst, &mut offset, &mut var_offset_map),
                ),
                _ => inst.clone(),
            })
            .collect::<Vec<_>>();

        Program { instructions }
    }
}

impl From<ir::Program> for Program {
    fn from(program: ir::Program) -> Self {
        Program {
            instructions: from_ir_instructions(program.instructions),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\t.globl main")?;
        writeln!(f, "main:")?;
        for instr in &self.instructions {
            writeln!(f, "{}", instr)?
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand),
    Call(Identifier),
    Push(Operand),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov(src, dst) => write!(f, "\tmovl\t{src}, {dst}"),
            Instruction::Call(fn_name) => write!(f, "\tcall\t{fn_name}"),
            Instruction::Push(op) => write!(f, "\tpushl\t{op}"),
        }
    }
}

fn from_ir_instructions(instructions: Vec<ir::Instruction>) -> Vec<Instruction> {
    instructions
        .into_iter()
        .flat_map(Into::<Vec<_>>::into)
        .collect::<Vec<_>>()
}

const ARG_REGISTERS: [Reg; 6] = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];

impl From<ir::Instruction> for Vec<Instruction> {
    fn from(inst: ir::Instruction) -> Self {
        match inst {
            ir::Instruction::Copy(src, dst) => vec![Instruction::Mov(src.into(), dst.into())],
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
            Operand::Pseudo(name) => unreachable!("attempt to serialize pseudo {name}"),
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
