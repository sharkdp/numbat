#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Op {
    PushConstant,
    Add,
    Negate,
    Print,
    Exit,
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

    pub fn run(&mut self) {
        loop {
            let op = self.bytecode[self.ip];
            self.ip += 1;
            let op = unsafe { std::mem::transmute::<u8, Op>(op) };
            println!("OP: {:?}", op);
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
