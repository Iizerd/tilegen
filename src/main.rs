/*

uint64_t add64_r1_r1_r2(uint64_t r1, uint64_t r2) {
    return r1 + r2;
}

So the syntax is:

uint64_t <opname>_<dest_reg>(_<op_reg>)*(arguments) {

}

*/

use smallvec::{smallvec, SmallVec};

pub const X86_REGS: [&'static str; 16] = [
    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13",
    "r14", "r15",
];
pub const ARM_REGS: [&'static str; 16] = [
    "x9", "x3", "x2", "x19", "sp", "x20", "x1", "x0", "x4", "x5", "x10", "x11", "x21", "x22",
    "x23", "x24",
];


#[derive(Debug, Clone, Copy)]
enum Operand {
    Register(u8), // r#
    Immediate,    // imm    x7
    Memory,       // mem    x6
    Flags,        // flags  x27
    Rip,          // pc     x6
}
impl Operand {
    pub fn operand_name(&self) -> &'static str {
        match self {
            Operand::Register(index) => X86_REGS[*index as usize],
            Operand::Immediate => "imm",
            Operand::Memory => "mem",
            Operand::Flags => "flags",
            Operand::Rip => "pc",
        }
    }
    pub fn convention_name(&self) -> &'static str {
        match self {
            Operand::Register(index) => ARM_REGS[*index as usize],
            Operand::Immediate => "x7",
            Operand::Memory => "x6",
            Operand::Flags => "x27",
            Operand::Rip => "x6",
        }
    }
}
#[derive(Debug, Clone)]
struct Function {
    /// All the operands of the function, the one in the 0th position is the
    /// return value operand.
    pub operands: SmallVec<[Operand; 10]>,
    /// The arguments...
    pub arguments: Vec<String>,
    /// Generic name of the function.
    pub name: String,
    ///
    pub body: String,
}
impl Function {
    pub fn emit(mut self, dest: &mut String) {
        let mut x86_regs: SmallVec<[&'static str; 10]> = SmallVec::default();
        let mut arm_regs: SmallVec<[&'static str; 10]> = SmallVec::default();
        let mut cycling_map: SmallVec<[(u8, SmallVec<[u8; 10]>); 5]> = SmallVec::default();
        for (i, op) in self.operands.iter().enumerate() {
            x86_regs.push(op.operand_name());
            arm_regs.push(op.convention_name());
            if let Operand::Register(reg_index) = op {
                let mut found = false;
                for (check_index, arg_indices) in cycling_map.iter_mut() {
                    if *check_index == *reg_index {
                        arg_indices.push(i as u8);
                        found = true;
                        break;
                    }
                }
                if !found {
                    cycling_map.push((*reg_index, smallvec![i as u8]));
                }
            }
        }

        for i in 0..16usize.pow(cycling_map.len() as u32) {
            for (cycle_index, (_, arg_indices)) in cycling_map.iter().enumerate() {
                let reg_index = (i >> (cycle_index * 4)) & 0b1111;
                for &arg in arg_indices.iter() {
                    x86_regs[arg as usize] = X86_REGS[reg_index];
                    arm_regs[arg as usize] = ARM_REGS[reg_index];
                }
            }
            dest.push_str("uint64_t ");
            dest.push_str(&self.name);
            for reg in x86_regs.iter() {
                dest.push('_');
                dest.push_str(reg);
            }
            dest.push_str("@<");
            dest.push_str(arm_regs[0]);
            dest.push_str(">(");
            for (arg, reg) in self.arguments.iter().zip(arm_regs[1..].iter()) {
                dest.push_str("uint64_t ");
                dest.push_str(&arg);
                dest.push_str("@<");
                dest.push_str(reg);
                dest.push_str(">, ");
            }
            dest.pop();
            dest.pop();
            dest.push(')');
            dest.push_str(&self.body);
            dest.push('\n');
        }
    }
}

struct Parser {
    chars: Vec<char>,
    loc: usize,
}
impl Parser {
    pub fn new(file: &String) -> Self {
        Self {
            chars: file.chars().collect(),
            loc: 0,
        }
    }
    #[inline(always)]
    pub fn at_end(&self) -> bool {
        self.loc >= self.chars.len()
    }
    #[inline(always)]
    pub fn expect_string(&mut self, s: &str) {
        for (c1, c2) in s.chars().zip(self.chars[self.loc..].iter()) {
            if c1 != *c2 {
                panic!("Did not find expected string {} at: {}", s, self.loc);
            }
        }
        self.loc += s.len();
    }
    #[inline(always)]
    pub fn skip_until_string(&mut self, s: &str) -> bool {
        for (c1, c2) in s.chars().zip(self.chars[self.loc..].iter()) {
            if c1 != *c2 {
                return false;
            }
        }
        true
    }

    pub fn read_funcname(&mut self) -> String {
        let mut result = String::with_capacity(10);

        while !self.at_end() && self.chars[self.loc].is_ascii_alphanumeric()
            || '_' == self.chars[self.loc]
        {
            result.push(self.chars[self.loc]);
            self.loc += 1;
        }

        result
    }
    pub fn to_non_whitespace(&mut self) {
        while !self.at_end() && self.chars[self.loc].is_whitespace() {
            self.loc += 1;
        }
    }
    pub fn to_next_function(&mut self) {
        while !self.at_end() && !self.skip_until_string("uint64_t") {
            self.loc += 1;
        }
    }
    // Operands are
    fn parse_name_operand(arg: &str) -> Operand {
        let first_char = arg.chars().next().unwrap();

        match first_char {
            'r' => Operand::Register(arg[1..].parse().unwrap()),
            'i' => Operand::Immediate,
            'm' => Operand::Memory,
            'f' => Operand::Flags,
            'p' => Operand::Rip,
            _ => panic!("Invalid operand: {}", arg),
        }
    }

    pub fn parse_function(&mut self) -> Function {
        // Eat the return value.
        self.expect_string("uint64_t");
        // Skip to name.
        self.to_non_whitespace();

        let func_name = self.read_funcname();

        // Find the opening paren for the arguments.
        self.expect_string("(");
        // Read until the last one.
        let mut arguments = String::with_capacity(30);
        while !self.at_end() && ')' != self.chars[self.loc] {
            arguments.push(self.chars[self.loc]);
            self.loc += 1;
        }
        self.loc += 1;

        let mut result = Function {
            operands: SmallVec::default(),
            arguments: Vec::with_capacity(arguments.len() / 2),
            name: String::default(),
            body: String::with_capacity(100),
        };

        let func_name_parts: Vec<&str> = func_name.split("_").collect();
        if func_name_parts.len() < 2 {
            panic!(
                "Error at {}, not enough parts in the function name: {}",
                self.loc, func_name
            );
        }

        result.name = func_name_parts[0].to_owned();

        for part in func_name_parts[1..].iter() {
            result.operands.push(Self::parse_name_operand(part));
        }

        let mut buffer = String::with_capacity(20);
        // Parse the arguments chars to get the argument names.
        for ch in arguments.chars() {
            if ch.is_alphanumeric() || '_' == ch {
                buffer.push(ch);
            } else if !buffer.is_empty() {
                if "uint64_t" != buffer {
                    result.arguments.push(buffer.clone());
                }
                buffer.clear();
            }
        }
        if !buffer.is_empty() {
            if "uint64_t" != buffer {
                result.arguments.push(buffer.clone());
                buffer.clear();
            }
        }
        // Read until the first {
        while !self.at_end() && '{' != self.chars[self.loc] {
            self.loc += 1;
        }
        result.body.push('{');
        // Eat the '{'
        self.loc += 1;
        // Loop until braces are balanced.
        let mut brace_bal = 1;
        while !self.at_end() && 0 != brace_bal {
            match self.chars[self.loc] {
                '{' => brace_bal += 1,
                '}' => brace_bal -= 1,
                _ => {}
            }
            result.body.push(self.chars[self.loc]);
            self.loc += 1;
        }
        // eat final '}'
        self.loc += 1;

        if result.operands.len() - 1 != result.arguments.len() {
            panic!("Invalid number of operands vs arguments: {:?}", result);
        }

        result
    }
    fn parse_functions(mut self) -> Vec<Function> {
        let mut result = Vec::default();
        while !self.at_end() {
            self.to_next_function();
            if self.at_end() {
                break;
            }
            result.push(self.parse_function());
        }
        result
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if 2 != args.len() {
        println!("Specify file please.");
        std::process::exit(1);
    }

    let contents =
        std::fs::read_to_string(args[1].clone()).expect("Could not read contents of file.");

    let functions = Parser::new(&contents).parse_functions();

    let mut result = String::with_capacity(20000);

    result.push_str("#include <stdint.h>\n\n/*---------------AUTOGENERATED BY TILEGEN PROGRAM---------------*/\n\n");

    for func in functions {
        func.emit(&mut result);
    }

    println!("{}", result);

}
