use std::{collections::HashMap, fmt};

use crate::{
    ir::{self},
    Identifier, Span,
};

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
        let instructions = self
            .instructions
            .clone()
            .into_iter()
            .flat_map(|inst| match inst {
                Instruction::Mov(src, dst, span) => match (src, dst) {
                    (Operand::Stack(src, src_span), Operand::Stack(dst, dst_span)) => {
                        vec![
                            Instruction::Mov(
                                Operand::Stack(src, src_span),
                                Operand::Reg(Reg::R10, None),
                                span,
                            ),
                            Instruction::Mov(
                                Operand::Reg(Reg::R10, None),
                                Operand::Stack(dst, dst_span),
                                None,
                            ),
                        ]
                    }
                    (src, dst) => vec![Instruction::Mov(src, dst, span)],
                },
                Instruction::Binary(op, ref src, ref dst, span) => match (op, src, dst) {
                    (
                        BinaryOperator::Add,
                        Operand::Stack(src, src_span),
                        Operand::Stack(dst, dst_span),
                    )
                    | (
                        BinaryOperator::Sub,
                        Operand::Stack(src, src_span),
                        Operand::Stack(dst, dst_span),
                    ) => {
                        vec![
                            Instruction::Mov(
                                Operand::Stack(*src, *src_span),
                                Operand::Reg(Reg::R10, None),
                                span,
                            ),
                            Instruction::Binary(
                                op,
                                Operand::Reg(Reg::R10, None),
                                Operand::Stack(*dst, *dst_span),
                                None,
                            ),
                        ]
                    }
                    (BinaryOperator::Mul, src, Operand::Stack(dst, dst_span)) => {
                        vec![
                            Instruction::Mov(
                                Operand::Stack(*dst, *dst_span),
                                Operand::Reg(Reg::R11, None),
                                span,
                            ),
                            Instruction::Binary(
                                op,
                                src.clone(),
                                Operand::Reg(Reg::R11, None),
                                None,
                            ),
                            Instruction::Mov(
                                Operand::Reg(Reg::R11, None),
                                Operand::Stack(*dst, *dst_span),
                                None,
                            ),
                        ]
                    }
                    _ => vec![inst],
                },
                Instruction::Idiv(operand, span) => vec![
                    Instruction::Mov(operand, Operand::Reg(Reg::R10, None), span),
                    Instruction::Idiv(Operand::Reg(Reg::R10, None), None),
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
                Operand::Pseudo(name, span) => {
                    if let Some(offset) = var_offset_map.get(&name.0) {
                        Operand::Stack(*offset, *span)
                    } else {
                        *offset -= 4;
                        var_offset_map.insert(name.0.clone(), *offset);
                        Operand::Stack(*offset, *span)
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
                Instruction::Mov(src, dst, span) => Instruction::Mov(
                    handle_operand(src, &mut offset, &mut var_offset_map),
                    handle_operand(dst, &mut offset, &mut var_offset_map),
                    *span,
                ),
                Instruction::Binary(op, src1, src2, span) => Instruction::Binary(
                    *op,
                    handle_operand(src1, &mut offset, &mut var_offset_map),
                    handle_operand(src2, &mut offset, &mut var_offset_map),
                    *span,
                ),
                Instruction::Unary(op, src, span) => Instruction::Unary(
                    *op,
                    handle_operand(src, &mut offset, &mut var_offset_map),
                    *span,
                ),
                _ => inst.clone(),
            })
            .collect::<Vec<_>>();

        let mut instructions = Vec::new();
        let aligned_stack_space = ((-offset + 15) / 16) * 16;

        instructions.push(Instruction::AllocateStack(aligned_stack_space, None));
        instructions.extend(tmp_instructions);
        instructions.push(Instruction::DeallocateStack(aligned_stack_space, None));

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

pub trait ToCode {
    fn to_code(&self, source: impl ToString, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

pub struct CodeDisplay<'a, T: ToCode> {
    inner: &'a T,
    source: String,
}

impl<'a, T: ToCode> CodeDisplay<'a, T> {
    pub fn new(inner: &'a T, source: impl ToString) -> Self {
        CodeDisplay {
            inner,
            source: source.to_string(),
        }
    }
}

impl<'a, T: ToCode> fmt::Display for CodeDisplay<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.to_code(&self.source, f)
    }
}

pub fn to_code<T: ToCode>(value: &T, source: impl ToString) -> CodeDisplay<'_, T> {
    CodeDisplay::new(value, source)
}

impl ToCode for Program {
    fn to_code(&self, source: impl ToString, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\t.globl main")?;
        writeln!(f, "main:")?;
        writeln!(f, "\tpushq\t%rbp")?;
        writeln!(f, "\tmovq\t%rsp, %rbp")?;
        for instr in &self.instructions {
            // if let Some(span) = instr.span() {
            //     writeln!(f, "\t# {}", span.value(&source.to_string()))?;
            // }
            writeln!(f, "{}", to_code(instr, source.to_string()))?;
        }
        writeln!(f, "\tpopq\t%rbp")?;
        writeln!(f, "\tret")?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand, Option<Span>),
    Call(Identifier, Option<Span>),
    Push(Operand, Option<Span>),
    AllocateStack(i32, Option<Span>),
    DeallocateStack(i32, Option<Span>),
    Binary(BinaryOperator, Operand, Operand, Option<Span>),
    Unary(UnaryOperator, Operand, Option<Span>),
    Cdq(Option<Span>),
    Idiv(Operand, Option<Span>),
    SpanOnly(Span),
}

impl Instruction {
    pub fn span(&self) -> Option<Span> {
        match self {
            Instruction::Mov(_, _, span) => *span,
            Instruction::Call(_, span) => *span,
            Instruction::Push(_, span) => *span,
            Instruction::AllocateStack(_, span) => *span,
            Instruction::DeallocateStack(_, span) => *span,
            Instruction::Binary(_, _, _, span) => *span,
            Instruction::Unary(_, _, span) => *span,
            Instruction::Cdq(span) => *span,
            Instruction::Idiv(_, span) => *span,
            Instruction::SpanOnly(span) => Some(*span),
        }
    }
}

impl ToCode for Instruction {
    fn to_code(&self, code: impl ToString, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // let code = code.to_string();
        match self {
            Instruction::Mov(src, dst, _span) => write!(f, "\tmovl\t{src}, {dst}",),
            Instruction::Call(fn_name, _span) => write!(f, "\tcall\t{fn_name}"),
            Instruction::Push(op, _span) => write!(f, "\tpushl\t{op}"),
            Instruction::AllocateStack(offset, _span) => write!(f, "\tsubq\t${offset}, %rsp"),
            Instruction::DeallocateStack(offset, _span) => write!(f, "\taddq\t${offset}, %rsp"),
            Instruction::Binary(op, src1, src2, _span) => write!(f, "\t{op}\t{src1}, {src2}"),
            Instruction::Unary(op, src, _span) => write!(f, "\t{op}\t{src}"),
            Instruction::Cdq(_span) => write!(f, "\tcdq"),
            Instruction::Idiv(op, _span) => write!(f, "\tidivl\t{op}"),
            Instruction::SpanOnly(span) => write!(f, "\n\t# {}", span.value(&code.to_string())),
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
            ir::Instruction::SpanOnly(span) => {
                vec![Instruction::SpanOnly(span)]
            }
            ir::Instruction::Copy(src, dst, span) => {
                vec![Instruction::Mov(src.into(), dst.into(), span)]
            }
            ir::Instruction::FunCall {
                name,
                args,
                result,
                span: _,
            } => {
                let mut instructions = Vec::new();

                let args = args.into_iter().map(Operand::from).collect::<Vec<_>>();
                let (register_args, stack_args) = if args.len() <= 6 {
                    (&args[..], &[][..])
                } else {
                    args.split_at(6)
                };
                let stack_padding = if stack_args.len() % 2 == 0 { 0 } else { 8 };

                if stack_padding > 0 {
                    instructions.push(Instruction::AllocateStack(stack_padding, None));
                }

                for (i, val) in register_args.iter().enumerate() {
                    instructions.push(Instruction::Mov(
                        val.clone(),
                        Operand::Reg(ARG_REGISTERS[i].clone(), None),
                        val.span(),
                    ));
                }

                for stack_arg in stack_args.iter().rev() {
                    instructions.push(Instruction::Push(stack_arg.clone(), stack_arg.span()));
                }

                let name = Identifier(name.0.to_lowercase());
                instructions.push(Instruction::Call(name, None));
                instructions.push(Instruction::Mov(
                    Operand::Reg(Reg::AX, None),
                    result.into(),
                    None,
                ));

                let bytes_to_remove = 8 * stack_args.len() as i32 + stack_padding;
                if bytes_to_remove > 0 {
                    instructions.push(Instruction::DeallocateStack(bytes_to_remove, None));
                }

                instructions
            }
            ir::Instruction::Unary(op, src, dst, span) => match op {
                _ => vec![
                    Instruction::Mov(src.into(), dst.clone().into(), span),
                    Instruction::Unary(op.into(), dst.into(), None),
                ],
            },
            ir::Instruction::Binary(op, src1, src2, dst, span) => match op {
                ir::BinaryOperator::Add
                | ir::BinaryOperator::Subtract
                | ir::BinaryOperator::Multiply
                | ir::BinaryOperator::Exponent => vec![
                    Instruction::Mov(src1.into(), dst.clone().into(), span),
                    Instruction::Binary(op.into(), src2.into(), dst.into(), None),
                ],
                ir::BinaryOperator::Divide => vec![
                    Instruction::Mov(src1.into(), Operand::Reg(Reg::AX, None), span),
                    Instruction::Cdq(None),
                    Instruction::Idiv(src2.into(), None),
                    Instruction::Mov(Operand::Reg(Reg::AX, None), dst.into(), None),
                ],
                ir::BinaryOperator::Modulo => vec![
                    Instruction::Mov(src1.into(), Operand::Reg(Reg::AX, None), None),
                    Instruction::Cdq(None),
                    Instruction::Idiv(src2.into(), None),
                    Instruction::Mov(Operand::Reg(Reg::DX, None), dst.into(), None),
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
            ir::Instruction::Label(..) => todo!(),
            ir::Instruction::Jump(..) => todo!(),
            ir::Instruction::JumpIfZero(_, _, _) => todo!(),
            ir::Instruction::JumpIfNotZero(_, _, _) => todo!(),
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

impl ToCode for BinaryOperator {
    fn to_code(&self, _source: impl ToString, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "addl"),
            BinaryOperator::Sub => write!(f, "subl"),
            BinaryOperator::Mul => write!(f, "imull"),
            BinaryOperator::And => write!(f, "andl"),
            BinaryOperator::Or => write!(f, "orl"),
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

impl ToCode for UnaryOperator {
    fn to_code(&self, _source: impl ToString, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Neg => write!(f, "negl"),
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
    Imm(i32, Option<Span>),
    Pseudo(Identifier, Option<Span>),
    Reg(Reg, Option<Span>),
    Reg8(Reg8, Option<Span>),
    Stack(i32, Option<Span>),
}

impl Operand {
    pub fn span(&self) -> Option<Span> {
        match self {
            Operand::Imm(_, span) => *span,
            Operand::Pseudo(_, span) => *span,
            Operand::Reg(_, span) => *span,
            Operand::Reg8(_, span) => *span,
            Operand::Stack(_, span) => *span,
        }
    }
}

impl From<ir::Val> for Operand {
    fn from(val: ir::Val) -> Self {
        match val {
            ir::Val::Constant(val, span) => Operand::Imm(val, span),
            ir::Val::Var(name, span) => Operand::Pseudo(name, span),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Imm(val, _span) => write!(f, "${val}"),
            Operand::Pseudo(name, _span) => unreachable!("attempt to serialize pseudo {name}"),
            Operand::Reg(reg, _span) => write!(f, "{reg}"),
            Operand::Reg8(reg, _span) => write!(f, "{reg}"),
            Operand::Stack(offset, _span) => write!(f, "{offset}(%rbp)"),
        }
    }
}

impl ToCode for Operand {
    fn to_code(&self, _source: impl ToString, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Imm(val, _span) => write!(f, "${val}"),
            Operand::Pseudo(name, _span) => unreachable!("attempt to serialize pseudo {name}"),
            Operand::Reg(reg, _span) => write!(f, "{reg}"),
            Operand::Reg8(reg, _span) => write!(f, "{reg}"),
            Operand::Stack(offset, _span) => write!(f, "{offset}(%rbp)"),
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
    R11,
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
            Reg::R11 => write!(f, "%r11d"),
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
        let inst = ir::Instruction::Copy(
            ir::Val::Constant(2, None),
            ir::Val::Var("nVar".into(), None),
            None,
        );
        let inst: Vec<Instruction> = inst.into();

        assert_eq!(
            inst[0],
            Instruction::Mov(
                Operand::Imm(2, None),
                Operand::Pseudo("nVar".into(), None),
                None
            )
        );
    }
}
