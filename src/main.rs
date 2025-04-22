use smallvec::{smallvec, SmallVec};

pub const X86_REGS: [&'static str; 16] = [
    "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8", "R9", "R10", "R11", "R12", "R13",
    "R14", "R15",
];
pub const N_X86_REGS: [&'static str; 16] = [
    "NRAX", "NRCX", "NRDX", "NRBX", "NRSP", "NRBP", "NRSI", "NRDI", "NR8", "NR9", "NR10", "NR11",
    "NR12", "NR13", "NR14", "NR15",
];
pub const ARM_REGS: [&'static str; 16] = [
    "X9", "X3", "X2", "X19", "X25", "X20", "X1", "X0", "X4", "X5", "X10", "X11", "X21", "X22",
    "X23", "X24",
];
pub const XMM_REGS: [&'static str; 16] = [
    "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", "XMM6", "XMM7", 
    "XMM8", "XMM9", "XMM10", "XMM11", "XMM12", "XMM13", "XMM14", "XMM15",
];
pub const NXMM_REGS: [&'static str; 16] = [
    "NXMM0", "NXMM1", "NXMM2", "NXMM3", "NXMM4", "NXMM5", "NXMM6", "NXMM7", 
    "NXMM8", "NXMM9", "NXMM10", "NXMM11", "NXMM12", "NXMM13", "NXMM14", "NXMM15",
];
pub const Q_REGS: [&'static str; 16] = [
    "Q0", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", 
    "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23",
];

// New enum to represent the different vector types
#[derive(Debug, Clone, Copy, PartialEq)]
enum VectorType {
    Scalar,      // uint64_t
    Int8x16,     // int8x16_t
    Int16x8,     // int16x8_t
    Int32x4,     // int32x4_t
    Int64x2,     // int64x2_t
    Uint8x16,    // uint8x16_t
    Uint16x8,    // uint16x8_t
    Uint32x4,    // uint32x4_t
    Uint64x2,    // uint64x2_t
    Float32x4,   // float32x4_t
    Float64x2,   // float64x2_t
    Float16x8,   // float16x8_t
    BFloat16x8,  // bfloat16x8_t
}

impl VectorType {
    fn to_type_string(&self) -> &'static str {
        match self {
            VectorType::Scalar => "uint64_t",
            VectorType::Int8x16 => "int8x16_t",
            VectorType::Int16x8 => "int16x8_t",
            VectorType::Int32x4 => "int32x4_t",
            VectorType::Int64x2 => "int64x2_t",
            VectorType::Uint8x16 => "uint8x16_t",
            VectorType::Uint16x8 => "uint16x8_t",
            VectorType::Uint32x4 => "uint32x4_t",
            VectorType::Uint64x2 => "uint64x2_t",
            VectorType::Float32x4 => "float32x4_t",
            VectorType::Float64x2 => "float64x2_t",
            VectorType::Float16x8 => "float16x8_t",
            VectorType::BFloat16x8 => "bfloat16x8_t",
        }
    }
    
    fn from_string(s: &str) -> Option<Self> {
        match s {
            "uint64_t" => Some(VectorType::Scalar),
            "int8x16_t" => Some(VectorType::Int8x16),
            "int16x8_t" => Some(VectorType::Int16x8),
            "int32x4_t" => Some(VectorType::Int32x4),
            "int64x2_t" => Some(VectorType::Int64x2),
            "uint8x16_t" => Some(VectorType::Uint8x16),
            "uint16x8_t" => Some(VectorType::Uint16x8),
            "uint32x4_t" => Some(VectorType::Uint32x4),
            "uint64x2_t" => Some(VectorType::Uint64x2),
            "float32x4_t" => Some(VectorType::Float32x4),
            "float64x2_t" => Some(VectorType::Float64x2),
            "float16x8_t" => Some(VectorType::Float16x8),
            "bfloat16x8_t" => Some(VectorType::BFloat16x8),
            "__mm128" => Some(VectorType::Int64x2), // Map to int64x2_t for compatibility
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Operand {
    Register(u8), // r#
    FixedRegister(u8),
    VectorRegister(u8, VectorType),  // xmm# with type
    FixedVectorRegister(u8, VectorType), // nxmm# with type
    Imm,   // x26
    Src,   // mem source x7
    Dst,   // mem dest x6
    Org,   // x6
    Flags, // x27
    XImm(VectorType),  // Vector immediate with type
    XSrc(VectorType),  // Vector source with type
    XDst(VectorType),  // Vector destination with type
    XOrg(VectorType),  // Vector original with type
}

impl Operand {
    pub fn operand_name(&self) -> &'static str {
        match self {
            Operand::Register(index) => X86_REGS[*index as usize],
            Operand::FixedRegister(index) => N_X86_REGS[*index as usize],
            Operand::VectorRegister(index, _) => XMM_REGS[*index as usize],
            Operand::FixedVectorRegister(index, _) => NXMM_REGS[*index as usize],
            Operand::Imm => "IMM",
            Operand::Src => "Tsrc",
            Operand::Dst => "Tdst",
            Operand::Org => "Torg",
            Operand::Flags => "FLAGS",
            Operand::XImm(_) => "XImm",
            Operand::XSrc(_) => "Xsrc",
            Operand::XDst(_) => "Xdst",
            Operand::XOrg(_) => "Xorg",
        }
    }
    
    pub fn convention_name(&self) -> &'static str {
        match self {
            Operand::Register(index) => ARM_REGS[*index as usize],
            Operand::FixedRegister(index) => ARM_REGS[*index as usize],
            Operand::VectorRegister(index, _) => Q_REGS[*index as usize],
            Operand::FixedVectorRegister(index, _) => Q_REGS[*index as usize],
            Operand::Imm => "X26",
            Operand::Dst | Operand::Org => "X6",
            Operand::Src => "X7",
            Operand::Flags => "X27",
            Operand::XImm(_) => "Q24",
            Operand::XSrc(_) => "Q25",
            Operand::XOrg(_) => "Q26",
            Operand::XDst(_) => "Q27",
        }
    }
    
    pub fn get_type(&self) -> VectorType {
        match self {
            Operand::Register(_) | 
            Operand::FixedRegister(_) | 
            Operand::Imm | 
            Operand::Src |
            Operand::Dst |
            Operand::Org |
            Operand::Flags => VectorType::Scalar,
            
            Operand::VectorRegister(_, ty) |
            Operand::FixedVectorRegister(_, ty) |
            Operand::XImm(ty) |
            Operand::XSrc(ty) |
            Operand::XDst(ty) |
            Operand::XOrg(ty) => *ty,
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
    /// Return type of the function
    pub return_type: String,
}

impl Function {
    #[allow(dead_code)]
    pub fn emit(self, dest: &mut String) {
        let mut x86_regs: SmallVec<[&'static str; 10]> = SmallVec::default();
        let mut arm_regs: SmallVec<[&'static str; 10]> = SmallVec::default();
        let mut cycling_map: SmallVec<[(u8, SmallVec<[u8; 10]>); 5]> = SmallVec::default();
        for (i, op) in self.operands.iter().enumerate() {
            x86_regs.push(op.operand_name());
            arm_regs.push(op.convention_name());
            match op {
                Operand::Register(reg_index) => {
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
                },
                Operand::VectorRegister(reg_index, _) => {
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
                },
                _ => {}
            }
        }

        for i in 0..16usize.pow(cycling_map.len() as u32) {
            for (cycle_index, (_, arg_indices)) in cycling_map.iter().enumerate() {
                let reg_index = (i >> (cycle_index * 4)) & 0b1111;
                for &arg in arg_indices.iter() {
                    match self.operands[arg as usize] {
                        Operand::Register(_) => {
                            x86_regs[arg as usize] = X86_REGS[reg_index];
                            arm_regs[arg as usize] = ARM_REGS[reg_index];
                        },
                        Operand::VectorRegister(_, _) => {
                            x86_regs[arg as usize] = XMM_REGS[reg_index];
                            arm_regs[arg as usize] = Q_REGS[reg_index];
                        },
                        _ => {}
                    }
                }
            }
            dest.push_str(&self.return_type);
            dest.push(' ');
            dest.push_str(&self.name);
            for reg in x86_regs.iter() {
                dest.push('_');
                dest.push_str(reg);
            }
            dest.push_str("@<");
            dest.push_str(arm_regs[0]);
            dest.push_str(">(");
            for (arg, reg) in self.arguments.iter().zip(arm_regs[1..].iter()) {
                // Determine the type based on the register and operand type
                let arg_type = if let Some(op_idx) = arm_regs[1..].iter().position(|r| r == reg) {
                    let op_type = self.operands[op_idx + 1].get_type();
                    op_type.to_type_string()
                } else if reg.starts_with("Q") {
                    // Fallback for Q registers
                    "int64x2_t"
                } else {
                    // Fallback for X registers
                    "uint64_t"
                };
                
                dest.push_str(arg_type);
                dest.push(' ');
                dest.push_str(&arg);
                dest.push_str("@<");
                dest.push_str(reg);
                dest.push_str(">, ");
            }
            if !self.arguments.is_empty() {
                dest.pop();
                dest.pop();
            }
            dest.push(')');
            dest.push_str(&self.body);
            dest.push('\n');
        }
    }
    
    pub fn emit2(self, dest: &mut String) {
        let mut x86_regs: SmallVec<[&'static str; 10]> = SmallVec::default();
        let mut arm_regs: SmallVec<[&'static str; 10]> = SmallVec::default();
        let mut cycling_map: SmallVec<[(u8, SmallVec<[u8; 10]>); 5]> = SmallVec::default();
        for (i, op) in self.operands.iter().enumerate() {
            x86_regs.push(op.operand_name());
            arm_regs.push(op.convention_name());
            match op {
                Operand::Register(reg_index) => {
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
                },
                Operand::VectorRegister(reg_index, _) => {
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
                },
                _ => {}
            }
        }

        for i in 0..16usize.pow(cycling_map.len() as u32) {
            for (cycle_index, (_, arg_indices)) in cycling_map.iter().enumerate() {
                let reg_index = (i >> (cycle_index * 4)) & 0b1111;
                for &arg in arg_indices.iter() {
                    match self.operands[arg as usize] {
                        Operand::Register(_) => {
                            x86_regs[arg as usize] = X86_REGS[reg_index];
                            arm_regs[arg as usize] = ARM_REGS[reg_index];
                        },
                        Operand::VectorRegister(_, _) => {
                            x86_regs[arg as usize] = XMM_REGS[reg_index];
                            arm_regs[arg as usize] = Q_REGS[reg_index];
                        },
                        _ => {}
                    }
                }
            }
            dest.push_str("__attribute__((aarch64_custom_reg(\"");
            dest.push_str(arm_regs[0]);
            dest.push_str(": ");
            for reg in arm_regs[1..].iter() {
                dest.push_str(reg);
                dest.push_str(", ");
            }
            if !arm_regs[1..].is_empty() {
                dest.pop();
                dest.pop();
            }
            dest.push_str("\"))) ");
            dest.push_str(&self.return_type);
            dest.push(' ');
            dest.push_str(&self.name);
            for reg in x86_regs.iter() {
                dest.push('_');
                dest.push_str(reg);
            }
            dest.push_str("(");
            for (i, arg) in self.arguments.iter().enumerate() {
                // Get the type from the corresponding operand
                let arg_type = self.operands[i+1].get_type().to_type_string();
                dest.push_str(arg_type);
                dest.push(' ');
                dest.push_str(&arg);
                dest.push_str(", ");
            }
            if !self.arguments.is_empty() {
                dest.pop();
                dest.pop();
            }
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
        if self.chars[self.loc..].len() < s.len() {
            return false;
        }
        
        for (_i, (c1, c2)) in s.chars().zip(self.chars[self.loc..self.loc+s.len()].iter()).enumerate() {
            if c1 != *c2 {
                return false;
            }
        }
        true
    }

    pub fn read_funcname(&mut self) -> String {
        let mut result = String::with_capacity(10);

        while !self.at_end() && (self.chars[self.loc].is_ascii_alphanumeric()
            || '_' == self.chars[self.loc])
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
        while !self.at_end() {
            // Check for all supported type declarations
            if self.skip_until_string("uint64_t") ||
               self.skip_until_string("int8x16_t") ||
               self.skip_until_string("int16x8_t") ||
               self.skip_until_string("int32x4_t") ||
               self.skip_until_string("int64x2_t") ||
               self.skip_until_string("uint8x16_t") ||
               self.skip_until_string("uint16x8_t") ||
               self.skip_until_string("uint32x4_t") ||
               self.skip_until_string("uint64x2_t") ||
               self.skip_until_string("float32x4_t") ||
               self.skip_until_string("float64x2_t") ||
               self.skip_until_string("float16x8_t") ||
               self.skip_until_string("bfloat16x8_t") ||
               self.skip_until_string("__mm128") {
                break;
            }
            self.loc += 1;
        }
    }

    fn match_native_reg(name: &str) -> u8 {
        for (i, reg) in X86_REGS.iter().enumerate() {
            if name == *reg {
                return i as u8;
            }
        }
        panic!("Invalid register name: {}", name);
    }
    
    #[allow(dead_code)]
    fn match_xmm_reg(name: &str) -> u8 {
        for (i, reg) in XMM_REGS.iter().enumerate() {
            if name == *reg {
                return i as u8;
            }
        }
        panic!("Invalid XMM register name: {}", name);
    }
    
    // Extract type suffix from name (e.g., "_i8x16", "_f32x4")
    fn extract_type_suffix(name: &str) -> Option<VectorType> {
        if name.contains("_i8x16") {
            Some(VectorType::Int8x16)
        } else if name.contains("_i16x8") {
            Some(VectorType::Int16x8)
        } else if name.contains("_i32x4") {
            Some(VectorType::Int32x4)
        } else if name.contains("_i64x2") {
            Some(VectorType::Int64x2)
        } else if name.contains("_u8x16") {
            Some(VectorType::Uint8x16)
        } else if name.contains("_u16x8") {
            Some(VectorType::Uint16x8)
        } else if name.contains("_u32x4") {
            Some(VectorType::Uint32x4)
        } else if name.contains("_u64x2") {
            Some(VectorType::Uint64x2)
        } else if name.contains("_f32x4") {
            Some(VectorType::Float32x4)
        } else if name.contains("_f64x2") {
            Some(VectorType::Float64x2)
        } else if name.contains("_f16x8") {
            Some(VectorType::Float16x8)
        } else if name.contains("_bf16x8") {
            Some(VectorType::BFloat16x8)
        } else {
            None
        }
    }

    // Parse operands with vector type information
    fn parse_name_operand(arg: &str, default_type: VectorType) -> Operand {
        let mut chars = arg.chars();
        let first_char = chars.next().unwrap().to_ascii_lowercase();
        
        // Check for type suffix in operand name
        let mut op_type = default_type;
        if let Some(type_suffix) = Self::extract_type_suffix(arg) {
            op_type = type_suffix;
            // Remove type suffix from arg for further processing
            // This would require more complex string parsing
        }
        
        match first_char {
            'r' => {
                if let Ok(index) = arg[1..].parse::<u8>() {
                    Operand::Register(index)
                } else {
                    panic!("Invalid register operand: {}", arg);
                }
            },
            'n' => {
                if arg.len() >= 4 && arg[0..4].to_ascii_lowercase() == "nxmm" {
                    // Handle NXMM registers
                    if let Ok(index) = arg[4..].parse::<u8>() {
                        Operand::FixedVectorRegister(index, op_type)
                    } else {
                        panic!("Invalid NXMM register operand: {}", arg);
                    }
                } else {
                    // Handle N registers
                    Operand::FixedRegister(Self::match_native_reg(&arg[1..]))
                }
            },
            'x' => {
                if arg.len() >= 3 && arg[0..3].to_ascii_lowercase() == "xmm" {
                    // Handle XMM registers
                    if let Ok(index) = arg[3..].parse::<u8>() {
                        Operand::VectorRegister(index, op_type)
                    } else {
                        panic!("Invalid XMM register operand: {}", arg);
                    }
                } else if arg.len() >= 2 {
                    let second_char = arg.chars().nth(1).unwrap();
                    
                    // Check if X is followed by a number (handle X0, X1, etc. as XMM registers)
                    if second_char.is_ascii_digit() {
                        if let Ok(index) = arg[1..].parse::<u8>() {
                            return Operand::VectorRegister(index, op_type);
                        } else {
                            panic!("Invalid XMM register: {}", arg);
                        }
                    }
                    
                    // Check for special vector operands with various syntaxes
                    let arg_lower = arg.to_ascii_lowercase();
                    if arg_lower == "ximm" || arg_lower == "xim" {
                        return Operand::XImm(op_type);
                    } else if arg_lower == "xsrc" || arg_lower == "xs" {
                        return Operand::XSrc(op_type);
                    } else if arg_lower == "xdst" || arg_lower == "xd" {
                        return Operand::XDst(op_type);
                    } else if arg_lower == "xorg" || arg_lower == "xo" {
                        return Operand::XOrg(op_type);
                    }
                    
                    // If it doesn't match any of our known patterns, panic
                    panic!("Invalid X operand: {}", arg);
                } else {
                    panic!("Invalid operand starting with 'x': {}", arg);
                }
            },
            'i' => Operand::Imm,
            't' => {
                let second_char = chars.next().unwrap_or(' ').to_ascii_lowercase();
                match second_char {
                    's' => Operand::Src,
                    'd' => Operand::Dst,
                    'o' => Operand::Org,
                    _ => panic!("Invalid operand starting with 't': {}", arg),
                }
            },
            'f' => Operand::Flags,
            _ => panic!("Invalid operand: {}", arg),
        }
    }

    pub fn parse_function(&mut self) -> Function {
        // Determine the return type
        let mut return_type_str = String::new();
        let mut return_type_enum: VectorType;
        
        // Check for all possible types in order
        let type_mappings = [
            ("uint64_t", VectorType::Scalar),
            ("int8x16_t", VectorType::Int8x16),
            ("int16x8_t", VectorType::Int16x8),
            ("int32x4_t", VectorType::Int32x4),
            ("int64x2_t", VectorType::Int64x2),
            ("uint8x16_t", VectorType::Uint8x16),
            ("uint16x8_t", VectorType::Uint16x8),
            ("uint32x4_t", VectorType::Uint32x4),
            ("uint64x2_t", VectorType::Uint64x2),
            ("float32x4_t", VectorType::Float32x4),
            ("float64x2_t", VectorType::Float64x2),
            ("float16x8_t", VectorType::Float16x8),
            ("bfloat16x8_t", VectorType::BFloat16x8),
            ("__mm128", VectorType::Int64x2),  // Map __mm128 to int64x2_t for backward compatibility
        ];
        
        let mut found_type = false;
        return_type_enum = VectorType::Scalar; // Default value
        
        for (type_str, type_enum) in type_mappings.iter() {
            if self.skip_until_string(type_str) {
                return_type_str = type_str.to_string();
                return_type_enum = *type_enum;
                self.expect_string(type_str);
                found_type = true;
                break;
            }
        }
        
        if !found_type {
            panic!("Could not determine return type at position {}", self.loc);
        }
        
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
            return_type: return_type_str,
        };

        let func_name_parts: Vec<&str> = func_name.split("_").collect();
        if func_name_parts.len() < 2 {
            panic!(
                "Error at {}, not enough parts in the function name: {}",
                self.loc, func_name
            );
        }

        result.name = func_name_parts[0].to_owned();

        // Check for type suffix in function name
        let mut default_type = return_type_enum;
        if let Some(type_suffix) = Self::extract_type_suffix(&func_name) {
            default_type = type_suffix;
        }

        for part in func_name_parts[1..].iter() {
            // Skip parts that are type indicators
            if Self::extract_type_suffix(part).is_some() {
                continue;
            }
            result.operands.push(Self::parse_name_operand(part, default_type));
        }

        let mut buffer = String::with_capacity(20);
        let mut in_type = false;
        
        // Parse the arguments chars to get the argument names and types.
        for ch in arguments.chars() {
            if ch.is_alphanumeric() || '_' == ch {
                buffer.push(ch);
            } else if !buffer.is_empty() {
                // Check if buffer is a type
                if VectorType::from_string(&buffer).is_some() {
                    in_type = true;
                } else if in_type {
                    result.arguments.push(buffer.clone());
                    in_type = false;
                }
                buffer.clear();
            }
        }
        if !buffer.is_empty() && in_type {
            result.arguments.push(buffer.clone());
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

    result.push_str("#include <stdint.h>\n#include \"EFLAG.h\"\n#include \"simd_types.h\"\n\n/*---------------AUTOGENERATED BY TILEGEN PROGRAM---------------*/\n\n");

    for func in functions {
        func.emit2(&mut result);
    }

    println!("{}", result);
}