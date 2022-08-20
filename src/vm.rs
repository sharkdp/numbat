#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Op {
    PushConstant,
    Add,
    Negate,
    Print,
    Exit,
}

impl Op {
    fn num_args(self) -> usize {
        match self {
            Op::PushConstant => 1,
            Op::Add | Op::Negate | Op::Print | Op::Exit => 0,
        }
    }

    fn to_string(self) -> &'static str {
        match self {
            Op::PushConstant => "PushConstant",
            Op::Add => "Add",
            Op::Negate => "Negate",
            Op::Print => "Print",
            Op::Exit => "Exit",
        }
    }
}

pub struct Vm {
    constants: Vec<f64>,
    bytecode: Vec<u8>,
    stack: Vec<f64>,
    ip: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            constants: vec![],
            bytecode: vec![],
            stack: vec![],
            ip: 0,
        }
    }

    pub fn disassemble(&self) {
        let mut i = 0;
        while i < self.bytecode.len() {
            let op = self.bytecode[i];
            let op = unsafe { std::mem::transmute::<u8, Op>(op) };

            let mut args = vec![];
            for _ in 0..op.num_args() {
                i += 1;
                args.push(self.bytecode[i]);
            }
            let args_str: String = args
                .iter()
                .map(|b| b.to_string())
                .collect::<Vec<String>>()
                .join(" ");

            print!("{:<15} {}", op.to_string(), args_str);
            if op == Op::PushConstant {
                print!("  (constant = {})", self.constants[args[0] as usize]);
            }
            println!();
            i += 1;
        }
    }

    pub fn run(&mut self) {
        loop {
            let op = self.bytecode[self.ip];
            self.ip += 1;
            let op = unsafe { std::mem::transmute::<u8, Op>(op) };
            // println!("OP: {:?}", op);
            match op {
                Op::PushConstant => {
                    let constant_idx = self.read_byte();
                    self.stack.push(self.constants[constant_idx as usize]);
                }
                Op::Add => {
                    let lhs = self.pop();
                    let rhs = self.pop();
                    self.stack.push(lhs + rhs);
                }
                Op::Negate => {
                    let rhs = self.pop();
                    self.stack.push(-rhs);
                }
                Op::Print => {
                    let byte = self.pop();
                    println!("{}", byte);
                }
                Op::Exit => {
                    break;
                }
            }
        }
    }

    fn pop(&mut self) -> f64 {
        self.stack.pop().expect("stack not empty")
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode[self.ip];
        self.ip += 1;
        byte
    }

    pub fn add_constant(&mut self, x: f64) -> u8 {
        self.constants.push(x);
        (self.constants.len() - 1) as u8 // TODO: this can overflow
    }

    pub fn add_op(&mut self, op: Op) {
        self.bytecode.push(op as u8);
    }

    pub fn add_op_with_arg(&mut self, op: Op, arg: u8) {
        self.bytecode.push(op as u8);
        self.bytecode.push(arg);
    }
}

#[test]
fn vm_basic() {
    let mut vm = Vm {
        constants: vec![42., 1.],
        #[rustfmt::skip]
        bytecode: vec![
            Op::PushConstant as u8, 0u8, // PUSH 0 (42.0)
            Op::PushConstant as u8, 1u8, // PUSH 1 (1.0)
            Op::Add as u8,               // ADD
            Op::Print as u8,             // PRINT
            Op::Exit as u8,              // EXIT
        ],
        stack: vec![],
        ip: 0,
    };
    vm.run();
}
