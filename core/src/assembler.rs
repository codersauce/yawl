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
        let program = program.fixup_instructions();

        Ok(program)
    }
}

pub struct Program {
    pub instructions: Vec<Instruction>,
}

impl Program {
    fn fixup_instructions(&self) -> Self {
        fn fixup(src: Operand, dst: Operand) -> Vec<(Operand, Operand)> {
            match (src, dst) {
                (Operand::Stack(src), Operand::Stack(dst)) => {
                    vec![
                        (Operand::Stack(src), Operand::Reg(Reg::R10)),
                        (Operand::Reg(Reg::R10), Operand::Stack(dst)),
                    ]
                }
                (src, dst) => vec![(src, dst)],
            }
        }

        let instructions = self
            .instructions
            .clone()
            .into_iter()
            .flat_map(|inst| match inst {
                Instruction::Mov(src, dst) => {
                    let pairs = fixup(src.clone(), dst.clone());
                    pairs
                        .into_iter()
                        .map(|(src, dst)| Instruction::Mov(src, dst))
                        .collect::<Vec<_>>()
                }
                Instruction::Binary(op, ref src, ref dst) => match op {
                    BinaryOperator::Add | BinaryOperator::Sub => {
                        let pairs = fixup(src.clone(), dst.clone());
                        pairs
                            .into_iter()
                            .map(|(src, dst)| Instruction::Binary(op, src, dst))
                            .collect::<Vec<_>>()
                    }
                    _ => vec![inst],
                },
                Instruction::Idiv(operand) => vec![
                    Instruction::Mov(operand, Operand::Reg(Reg::R10)),
                    Instruction::Idiv(Operand::Reg(Reg::R10)),
                ],
                _ => vec![inst],
            })
            .collect::<Vec<_>>();

        Program { instructions }
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
                Instruction::Binary(op, src1, src2) => Instruction::Binary(
                    *op,
                    handle_operand(src1, &mut offset, &mut var_offset_map),
                    handle_operand(src2, &mut offset, &mut var_offset_map),
                ),
                Instruction::Unary(op, src) => {
                    Instruction::Unary(*op, handle_operand(src, &mut offset, &mut var_offset_map))
                }
                _ => inst.clone(),
            })
            .collect::<Vec<_>>();

        let mut instructions = Vec::new();
        let aligned_stack_space = ((-offset + 15) / 16) * 16;

        instructions.push(Instruction::AllocateStack(aligned_stack_space));
        instructions.extend(tmp_instructions);
        instructions.push(Instruction::DellocateStack(aligned_stack_space));

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
        writeln!(f, "\tpushq\t%rbp")?;
        writeln!(f, "\tmovq\t%rsp, %rbp")?;
        for instr in &self.instructions {
            writeln!(f, "{}", instr)?
        }
        writeln!(f, "\tpopq\t%rbp")?;
        writeln!(f, "\tret")?;

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
    Binary(BinaryOperator, Operand, Operand),
    Unary(UnaryOperator, Operand),
    Cdq,
    Idiv(Operand),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov(src, dst) => write!(f, "\tmovl\t{src}, {dst}"),
            Instruction::Call(fn_name) => write!(f, "\tcall\t{fn_name}"),
            Instruction::Push(op) => write!(f, "\tpushl\t{op}"),
            Instruction::AllocateStack(offset) => write!(f, "\tsubq\t${offset}, %rsp"),
            Instruction::DellocateStack(offset) => write!(f, "\taddq\t${offset}, %rsp"),
            Instruction::Binary(op, src1, src2) => write!(f, "\t{op}\t{src1}, {src2}"),
            Instruction::Unary(op, src) => write!(f, "\t{op}\t{src}"),
            Instruction::Cdq => write!(f, "\tcdq"),
            Instruction::Idiv(op) => write!(f, "\tidivl\t{op}"),
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
            ir::Instruction::Unary(op, src, dst) => match op {
                _ => vec![
                    Instruction::Mov(src.into(), dst.clone().into()),
                    Instruction::Unary(op.into(), dst.into()),
                ],
            },
            ir::Instruction::Binary(op, src1, src2, dst) => match op {
                ir::BinaryOperator::Add
                | ir::BinaryOperator::Subtract
                | ir::BinaryOperator::Multiply
                | ir::BinaryOperator::Exponent => vec![
                    Instruction::Mov(src1.into(), dst.clone().into()),
                    Instruction::Binary(op.into(), src2.into(), dst.into()),
                ],
                ir::BinaryOperator::Divide => vec![
                    Instruction::Mov(src1.into(), Operand::Reg(Reg::AX)),
                    Instruction::Cdq,
                    Instruction::Idiv(src2.into()),
                    Instruction::Mov(Operand::Reg(Reg::AX), dst.into()),
                ],
                ir::BinaryOperator::Modulo => vec![
                    Instruction::Mov(src1.into(), Operand::Reg(Reg::AX)),
                    Instruction::Cdq,
                    Instruction::Idiv(src2.into()),
                    Instruction::Mov(Operand::Reg(Reg::DX), dst.into()),
                ],
                ir::BinaryOperator::And => todo!(),
                ir::BinaryOperator::Or => todo!(),
                ir::BinaryOperator::LessThan => todo!(),
                ir::BinaryOperator::LessThanEqual => todo!(),
                ir::BinaryOperator::GreaterThan => todo!(),
                ir::BinaryOperator::GreaterThanEqual => todo!(),
                ir::BinaryOperator::Equal => todo!(),
                ir::BinaryOperator::NotEqual => todo!(),
            },
            ir::Instruction::Label(_) => todo!(),
            ir::Instruction::Jump(_) => todo!(),
            ir::Instruction::JumpIfZero(_, _) => todo!(),
            ir::Instruction::JumpIfNotZero(_, _) => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    And,
    Or,
}

impl From<ir::BinaryOperator> for BinaryOperator {
    fn from(op: ir::BinaryOperator) -> Self {
        match op {
            ir::BinaryOperator::Add => BinaryOperator::Add,
            ir::BinaryOperator::Subtract => BinaryOperator::Sub,
            ir::BinaryOperator::Multiply => BinaryOperator::Mul,
            ir::BinaryOperator::And => BinaryOperator::And,
            ir::BinaryOperator::Or => BinaryOperator::Or,
            _ => todo!(),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "addl"),
            BinaryOperator::Sub => write!(f, "subl"),
            BinaryOperator::Mul => write!(f, "imull"),
            BinaryOperator::And => write!(f, "andl"),
            BinaryOperator::Or => write!(f, "orl"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Neg,
}

impl From<ir::UnaryOperator> for UnaryOperator {
    fn from(op: ir::UnaryOperator) -> Self {
        match op {
            ir::UnaryOperator::Negate => UnaryOperator::Neg,
            _ => todo!(),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Neg => write!(f, "negl"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Imm(i32),
    Pseudo(Identifier),
    Reg(Reg),
    Reg8(Reg8),
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
            Operand::Reg8(reg) => write!(f, "{reg}"),
            Operand::Stack(offset) => write!(f, "{offset}(%rsp)"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
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
            Reg::R10 => write!(f, "%r10d"),
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq)]
pub enum Reg8 {
    AL,
    CL,
    DL,
    DIL,
    SIL,
    R8B,
    R9B,
}

impl fmt::Display for Reg8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg8::AL => write!(f, "%al"),
            Reg8::CL => write!(f, "%cl"),
            Reg8::DL => write!(f, "%dl"),
            Reg8::DIL => write!(f, "%dil"),
            Reg8::SIL => write!(f, "%sil"),
            Reg8::R8B => write!(f, "%r8b"),
            Reg8::R9B => write!(f, "%r9b"),
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
