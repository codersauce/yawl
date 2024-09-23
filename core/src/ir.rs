use crate::{
    parser::{self, Exp, Statement},
    Identifier, Span,
};

pub struct IrGenerator {
    pub program: parser::Program,
}

impl IrGenerator {
    pub fn new(program: parser::Program) -> IrGenerator {
        Self { program }
    }

    pub fn generate(&mut self) -> anyhow::Result<Program> {
        let mut instructions = Vec::new();
        let mut context = Context::new();

        for st in &self.program.statements {
            instructions.push(Instruction::SpanOnly(st.span()));
            match st {
                Statement::Expression(exp, _span) => emit_ir(exp, &mut instructions, &mut context)?,
            };
        }

        Ok(Program { instructions })
    }
}

pub struct Program {
    pub instructions: Vec<Instruction>,
}

pub struct Context {
    next_num: usize,
    next_label: usize,
}

impl Context {
    fn new() -> Self {
        Context {
            next_num: 0,
            next_label: 0,
        }
    }

    fn next_var(&mut self) -> Identifier {
        let res = Identifier(format!("var_{}", self.next_num));
        self.next_num += 1;
        res
    }

    fn next_label(&mut self, label: &str) -> Identifier {
        let res = Identifier(format!("{label}_{}", self.next_label));
        self.next_label += 1;
        res
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    SpanOnly(Span),
    Copy(Val, Val, Option<Span>),
    FunCall {
        name: Identifier,
        args: Vec<Val>,
        result: Val,
        span: Option<Span>,
    },
    Unary(UnaryOperator, Val, Val, Option<Span>),
    Binary(BinaryOperator, Val, Val, Val, Option<Span>),
    Label(Identifier, Option<Span>),
    Jump(Identifier, Option<Span>),
    JumpIfZero(Val, Identifier, Option<Span>),
    JumpIfNotZero(Val, Identifier, Option<Span>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not,
    And,
    Positive,
    Negate,
    Increment,
    Decrement,
    Ref,
}

impl From<parser::UnaryOperator> for UnaryOperator {
    fn from(op: parser::UnaryOperator) -> Self {
        match op {
            parser::UnaryOperator::Not => UnaryOperator::Not,
            parser::UnaryOperator::And => UnaryOperator::And,
            parser::UnaryOperator::Positive => UnaryOperator::Positive,
            parser::UnaryOperator::Negative => UnaryOperator::Negate,
            parser::UnaryOperator::Increment => UnaryOperator::Increment,
            parser::UnaryOperator::Decrement => UnaryOperator::Decrement,
            parser::UnaryOperator::Ref => UnaryOperator::Ref,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponent,
    Modulo,
    And,
    Or,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
}

impl From<parser::BinaryOperator> for BinaryOperator {
    fn from(op: parser::BinaryOperator) -> Self {
        match op {
            parser::BinaryOperator::Add => BinaryOperator::Add,
            parser::BinaryOperator::Subtract => BinaryOperator::Subtract,
            parser::BinaryOperator::Multiply => BinaryOperator::Multiply,
            parser::BinaryOperator::Divide => BinaryOperator::Divide,
            parser::BinaryOperator::Exponent => BinaryOperator::Exponent,
            parser::BinaryOperator::Modulo => BinaryOperator::Modulo,
            parser::BinaryOperator::And => BinaryOperator::And,
            parser::BinaryOperator::Or => BinaryOperator::Or,
            parser::BinaryOperator::LessThan => BinaryOperator::LessThan,
            parser::BinaryOperator::LessThanEqual => BinaryOperator::LessThanEqual,
            parser::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
            parser::BinaryOperator::GreaterThanEqual => BinaryOperator::GreaterThanEqual,
            parser::BinaryOperator::Equal => BinaryOperator::Equal,
            parser::BinaryOperator::NotEqual => BinaryOperator::NotEqual,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Constant(i32, Option<Span>),
    Var(Identifier, Option<Span>),
}

pub fn emit_ir(
    exp: &Exp,
    instructions: &mut Vec<Instruction>,
    context: &mut Context,
) -> anyhow::Result<Val> {
    let val = match exp {
        Exp::Var(name, span) => Val::Var(name.to_string().into(), Some(*span)),
        Exp::Constant(val, span) => Val::Constant(*val, Some(*span)),
        Exp::Assignment(src, dst, assignment_span) => match src.as_ref() {
            Exp::Var(name, _span) => emit_assignment(
                name.clone(),
                dst.as_ref(),
                *assignment_span,
                instructions,
                context,
            )?,
            _expr => todo!(),
        },
        Exp::FunCall(name, args, span) => {
            let args = args
                .iter()
                .map(|arg| emit_ir(arg, instructions, context))
                .collect::<anyhow::Result<Vec<Val>>>()?;
            let result = Val::Var(context.next_var(), Some(*span));
            let name = Identifier(name.into());

            instructions.push(Instruction::FunCall {
                name,
                args,
                result: result.clone(),
                span: Some(*span),
            });

            result
        }
        Exp::Unary(op, exp, span) => {
            // TODO: Confirm what to do with items we generate
            let result = Val::Var(context.next_var(), None);
            let exp = emit_ir(exp, instructions, context)?;
            instructions.push(Instruction::Unary(
                op.clone().into(),
                exp,
                result.clone(),
                Some(*span),
            ));
            result
        }
        Exp::Binary(exp1, op, exp2, span) => match op {
            parser::BinaryOperator::Or => {
                instructions.push(Instruction::SpanOnly(*span));

                let dst = Val::Var(context.next_var(), None);

                let short_circuit_label = context.next_label("or_short_circuit");
                let end_label = context.next_label("end_label");

                // if exp1 is true, jump to short_circuit (result = true)
                let var1 = emit_ir(exp1, instructions, context)?;
                instructions.push(Instruction::JumpIfNotZero(
                    var1,
                    short_circuit_label.clone(),
                    None,
                ));

                // if exp2 is true, jump to short_circuit (result = true)
                let var2 = emit_ir(exp2, instructions, context)?;
                instructions.push(Instruction::JumpIfNotZero(
                    var2,
                    short_circuit_label.clone(),
                    None,
                ));

                // false case
                instructions.push(Instruction::Copy(Val::Constant(0, None), dst.clone(), None));
                instructions.push(Instruction::Jump(end_label.clone(), None));

                // short circuit case
                instructions.push(Instruction::Label(short_circuit_label, None));
                instructions.push(Instruction::Copy(Val::Constant(1, None), dst.clone(), None));

                instructions.push(Instruction::Label(end_label, None));

                dst
            }
            parser::BinaryOperator::And => {
                instructions.push(Instruction::SpanOnly(*span));

                let dst = Val::Var(context.next_var(), None);

                let short_circuit_label = context.next_label("or_short_circuit");
                let end_label = context.next_label("end_label");

                // if exp1 is false, jump to short_circuit (result = false)
                let var1 = emit_ir(exp1, instructions, context)?;
                instructions.push(Instruction::JumpIfZero(
                    var1,
                    short_circuit_label.clone(),
                    None,
                ));

                // if exp2 is false, jump to short_circuit (result = false)
                let var2 = emit_ir(exp2, instructions, context)?;
                instructions.push(Instruction::JumpIfZero(
                    var2,
                    short_circuit_label.clone(),
                    None,
                ));

                // true case
                instructions.push(Instruction::Copy(Val::Constant(1, None), dst.clone(), None));
                instructions.push(Instruction::Jump(end_label.clone(), None));

                // short circuit case
                instructions.push(Instruction::Label(short_circuit_label, None));
                instructions.push(Instruction::Copy(Val::Constant(0, None), dst.clone(), None));

                instructions.push(Instruction::Label(end_label, None));

                dst
            }
            _ => {
                let dst = Val::Var(context.next_var(), None);
                let exp1 = emit_ir(exp1, instructions, context)?;
                let exp2 = emit_ir(exp2, instructions, context)?;
                instructions.push(Instruction::Binary(
                    op.clone().into(),
                    exp1,
                    exp2,
                    dst.clone(),
                    Some(*span),
                ));
                dst
            }
        },
    };

    Ok(val)
}

fn emit_assignment(
    var_name: String,
    exp: &Exp,
    span: Span,
    instructions: &mut Vec<Instruction>,
    context: &mut Context,
) -> anyhow::Result<Val> {
    let val = emit_ir(exp, instructions, context)?;
    instructions.push(Instruction::Copy(
        val.clone(),
        Val::Var(var_name.into(), None),
        Some(span),
    ));
    Ok(val)
}
