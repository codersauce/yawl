use crate::{
    parser::{self, Exp, Statement},
    Identifier,
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
            match st {
                Statement::Expression(exp) => emit_ir(exp, &mut instructions, &mut context)?,
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
}

impl Context {
    fn new() -> Self {
        Context { next_num: 0 }
    }

    fn next_var(&mut self) -> Identifier {
        let res = Identifier(format!("var_{}", self.next_num));
        self.next_num += 1;
        res
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Copy(Val, Val),
    FunCall {
        name: Identifier,
        args: Vec<Val>,
        result: Val,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Constant(i32),
    Var(Identifier),
}

pub fn emit_ir(
    exp: &Exp,
    instructions: &mut Vec<Instruction>,
    context: &mut Context,
) -> anyhow::Result<Val> {
    let val = match exp {
        Exp::Var(name) => Val::Var(name.to_string().into()),
        Exp::Constant(val) => Val::Constant(*val),
        Exp::Assignment(src, dst) => match src.as_ref() {
            Exp::Var(name) => emit_assignment(name.clone(), dst.as_ref(), instructions, context)?,
            _expr => todo!(),
        },
        Exp::FunCall(name, args) => {
            let args = args
                .iter()
                .map(|arg| emit_ir(arg, instructions, context))
                .collect::<anyhow::Result<Vec<Val>>>()?;
            let result = Val::Var(context.next_var());
            let name = Identifier(name.into());

            instructions.push(Instruction::FunCall {
                name,
                args,
                result: result.clone(),
            });

            result
        }
        Exp::Unary(_, _) => todo!(),
        Exp::Binary(_, _, _) => todo!(),
    };

    Ok(val)
}

fn emit_assignment(
    var_name: String,
    exp: &Exp,
    instructions: &mut Vec<Instruction>,
    context: &mut Context,
) -> anyhow::Result<Val> {
    let val = emit_ir(exp, instructions, context)?;
    instructions.push(Instruction::Copy(val.clone(), Val::Var(var_name.into())));
    Ok(val)
}
