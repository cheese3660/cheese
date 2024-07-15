use num_bigint::BigInt;
use num_bigint::Sign::Plus;
use cheese_lexer::{Token, TokenType};
use crate::ast::{AstNode, DeclarationFlags, NodePtr};
use crate::ast::AstNodeData::{AddAssign, Addition, And, AndAssign, Assign, Cast, Combine, DivideAssign, Division, DynamicCast, EqualTo, GreaterEqual, GreaterThan, IsType, LeftShift, LesserEqual, LesserThan, ModulateAssign, Modulus, Multiplication, MultiplyAssign, NotEqualTo, Or, OrAssign, Range, RightShift, ShiftLeftAssign, ShiftRightAssign, SubtractAssign, Subtraction, Xor, XorAssign};

pub fn hex_to_big_int<T: ToString>(hex: T) -> BigInt {
    let mut bytes = vec![];
    let ts = hex.to_string();
    for ch in ts.chars().skip(2) {
        bytes.push(match ch {
            '0' => 0,
            '1' => 1,
            '2' => 2,
            '3' => 3,
            '4' => 4,
            '5' => 5,
            '6' => 6,
            '7' => 7,
            '8' => 8,
            '9' => 9,
            'a' | 'A' => 0xA,
            'b' | 'B' => 0xB,
            'c' | 'C' => 0xC,
            'd' | 'D' => 0xD,
            'e' | 'E' => 0xE,
            'f' | 'F' => 0xF,
            '_' => continue,
            _ => unreachable!()
        } as u8)
    }
    BigInt::from_radix_be(Plus,bytes.as_slice(),16).unwrap()
}

pub fn dec_to_big_int<T: ToString>(dec: T) -> BigInt {
    let mut bytes = vec![];
    let ts = dec.to_string();
    for ch in ts.chars() {
        bytes.push(match ch {
            '0' => 0,
            '1' => 1,
            '2' => 2,
            '3' => 3,
            '4' => 4,
            '5' => 5,
            '6' => 6,
            '7' => 7,
            '8' => 8,
            '9' => 9,
            '_' => continue,
            _ => unreachable!()
        } as u8)
    }
    BigInt::from_radix_be(Plus,bytes.as_slice(),10).unwrap()
}

pub fn oct_to_big_int<T: ToString>(oct: T) -> BigInt {
    let mut bytes = vec![];
    let ts = oct.to_string();
    for ch in ts.chars() {
        bytes.push(match ch {
            '0' => 0,
            '1' => 1,
            '2' => 2,
            '3' => 3,
            '4' => 4,
            '5' => 5,
            '6' => 6,
            '7' => 7,
            '_' => continue,
            _ => unreachable!()
        } as u8)
    }
    BigInt::from_radix_be(Plus,bytes.as_slice(),8).unwrap()
}

pub fn bin_to_big_int<T: ToString>(bin: T) -> BigInt {
    let mut bytes = vec![];
    let ts = bin.to_string();
    for ch in ts.chars() {
        bytes.push(match ch {
            '0' => 0,
            '1' => 1,
            '_' => continue,
            _ => unreachable!()
        } as u8)
    }
    BigInt::from_radix_be(Plus,bytes.as_slice(),2).unwrap()
}

pub fn is_statement_end(ty: TokenType) -> bool {
    match ty {
        TokenType::Semicolon | TokenType::Comma | TokenType::RightBrace | TokenType::RightBracket | TokenType::RightParentheses => true,
        _ => false
    }
}

pub fn is_binary_operation(ty: TokenType) -> bool {
    match ty {
        TokenType::Star
        | TokenType::Slash
        | TokenType::Ampersand
        | TokenType::Percent
        | TokenType::Plus
        | TokenType::Dash
        | TokenType::EqualTo
        | TokenType::NotEqualTo
        | TokenType::GreaterThanEqual
        | TokenType::GreaterThan
        | TokenType::LessThan
        | TokenType::LessThanEqual
        | TokenType::LeftShift
        | TokenType::RightShift
        | TokenType::Xor
        | TokenType::Or
        | TokenType::And
        | TokenType::Cast
        | TokenType::DynamicCast
        | TokenType::Is
        | TokenType::DoubleDot
        | TokenType::Assign
        | TokenType::AddAssign
        | TokenType::SubtractAssign
        | TokenType::MultiplyAssign
        | TokenType::DivideAssign
        | TokenType::ModuloAssign
        | TokenType::LeftShiftAssign
        | TokenType::RightShiftAssign
        | TokenType::XorAssign
        | TokenType::AndAssign
        | TokenType::OrAssign => true,
        _ => false
    }
}

pub fn is_binary_operand_type(ty: TokenType) -> bool {
    match ty {
        TokenType::Cast | TokenType::DynamicCast | TokenType::Is => true,
        _ => false
    }
}

pub fn precedence(ty: TokenType) -> u8 {
    match ty {
        TokenType::Star | TokenType::Slash | TokenType::Percent => 10,
        TokenType::Plus | TokenType::Dash => 9,
        TokenType::LeftShift | TokenType::RightShift => 8,
        TokenType::LessThanEqual | TokenType::GreaterThanEqual | TokenType::GreaterThan | TokenType::LessThan => 7,
        TokenType::EqualTo | TokenType::NotEqualTo | TokenType::DynamicCast | TokenType::Cast | TokenType::Is => 6,
        TokenType::And => 5,
        TokenType::Xor => 4,
        TokenType::Or => 3,
        TokenType::Ampersand => 2,
        TokenType::DoubleDot => 1,
        _ => 0
    }
}

pub fn create_node_from_binop(op: TokenType, lhs: NodePtr, rhs: NodePtr) -> NodePtr {
    AstNode::new(lhs.span.expanded(&rhs.span),match op {
        TokenType::Star => Multiplication { lhs, rhs },
        TokenType::Slash => Division { lhs, rhs },
        TokenType::Ampersand => Combine {lhs, rhs },
        TokenType::Percent => Modulus { lhs, rhs },
        TokenType::Plus => Addition { lhs, rhs },
        TokenType::Dash => Subtraction {lhs, rhs},
        TokenType::EqualTo => EqualTo {lhs, rhs},
        TokenType::NotEqualTo => NotEqualTo {lhs, rhs},
        TokenType::GreaterThanEqual => GreaterEqual {lhs, rhs},
        TokenType::LessThanEqual => LesserEqual {lhs, rhs},
        TokenType::GreaterThan => GreaterThan {lhs,rhs},
        TokenType::LessThan => LesserThan {lhs,rhs},
        TokenType::LeftShift => LeftShift {lhs, rhs},
        TokenType::RightShift => RightShift {lhs,rhs},
        TokenType::Xor => Xor {lhs, rhs},
        TokenType::Or => Or {lhs, rhs},
        TokenType::And => And {lhs,rhs},
        TokenType::Cast => Cast {lhs, to: rhs},
        TokenType::DynamicCast => DynamicCast {lhs, to: rhs},
        TokenType::Is => IsType {lhs, rhs},
        TokenType::DoubleDot => Range {begin: lhs, end: rhs},
        TokenType::Assign => Assign {lhs, rhs},
        TokenType::AddAssign => AddAssign {lhs, rhs},
        TokenType::SubtractAssign => SubtractAssign {lhs, rhs},
        TokenType::MultiplyAssign => MultiplyAssign {lhs, rhs},
        TokenType::DivideAssign => DivideAssign {lhs, rhs},
        TokenType::ModuloAssign => ModulateAssign {lhs, rhs},
        TokenType::LeftShiftAssign => ShiftLeftAssign{ lhs, rhs},
        TokenType::RightShiftAssign => ShiftRightAssign {lhs, rhs},
        TokenType::AndAssign => AndAssign {lhs, rhs},
        TokenType::OrAssign => OrAssign {lhs, rhs},
        TokenType::XorAssign => XorAssign {lhs, rhs},
        _ => unreachable!()
    })
}
pub fn builtin_remove_prefix<T: ToString>(builtin: T) -> String {
    builtin.to_string().trim_start_matches("$").to_string()
}
pub fn get_integer_type_size<T: ToString>(value: T) -> u64 {
    value.to_string().trim_start_matches("u").trim_start_matches("i").parse().unwrap()
}

pub fn is_flag(tt: TokenType) -> bool {
    match tt {
        TokenType::Inline => true,
        TokenType::Extern => true,
        TokenType::Export => true,
        TokenType::CompileTime => true,
        TokenType::Public => true,
        TokenType::Private => true,
        TokenType::Mutable => true,
        TokenType::Entry => true,
        TokenType::Implicit => true,
        TokenType::Explicit => true,
        _ => false
    }
}