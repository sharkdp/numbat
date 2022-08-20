#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Op {
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Print,
    Exit,
}

impl Op {
    fn num_operands(self) -> usize {
        match self {
            Op::Constant => 1,
            Op::Add
            | Op::Subtract
            | Op::Multiply
            | Op::Divide
            | Op::Negate
            | Op::Print
            | Op::Exit => 0,
        }
    }

    fn to_string(self) -> &'static str {
        match self {
            Op::Constant => "Constant",
            Op::Add => "Add",
            Op::Subtract => "Subtract",
            Op::Multiply => "Multiply",
            Op::Divide => "Divide",
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
        println!();
        println!(".CONSTANTS");
        for (idx, constant) in self.constants.iter().enumerate() {
            println!("  {:04} {}", idx, constant);
        }
        println!(".CODE");
        let mut offset = 0;
        while offset < self.bytecode.len() {
            let op = self.bytecode[offset];
            offset += 1;
            let op = unsafe { std::mem::transmute::<u8, Op>(op) };

            let operands = &self.bytecode[offset..(offset + op.num_operands())];
            offset += op.num_operands();

            let operands_str = operands
                .iter()
                .map(u8::to_string)
                .collect::<Vec<String>>()
                .join(" ");

            print!("  {:04} {:<10} {}", offset, op.to_string(), operands_str);

            if op == Op::Constant {
                print!("     (value: {})", self.constants[operands[0] as usize]);
            }
            println!();
        }
        println!();
    }

    pub fn run(&mut self) {
        loop {
            let op = self.bytecode[self.ip];
            self.ip += 1;
            let op = unsafe { std::mem::transmute::<u8, Op>(op) };
            match op {
                Op::Constant => {
                    let constant_idx = self.read_byte();
                    self.stack.push(self.constants[constant_idx as usize]);
                }
                Op::Add => {
                    let lhs = self.pop();
                    let rhs = self.pop();
                    self.stack.push(lhs + rhs);
                }
                Op::Multiply => {
                    let lhs = self.pop();
                    let rhs = self.pop();
                    self.stack.push(lhs * rhs);
                }
                Op::Subtract | Op::Divide => {}
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
            Op::Constant as u8, 0u8, // CONSTANT 0 (42.0)
            Op::Constant as u8, 1u8, // CONSTANT 1 (1.0)
            Op::Add as u8,           // ADD
            Op::Print as u8,         // PRINT
            Op::Exit as u8,          // EXIT
        ],
        stack: vec![],
        ip: 0,
    };
    vm.run();
}
