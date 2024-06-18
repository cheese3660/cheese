use std::str::FromStr;
use num_bigint::{BigInt, Sign};
use crate::ast::DeclarationFlags;
use crate::tests::{Error, v_empty_def, v_i, validate};
use crate::validation::{AstValidator, v_bool, v_false, v_integer_literal, v_none, v_program, v_string_literal, v_true, v_variable_declaration, v_variable_definition};
use crate::validation::AstValidator::IntegerLiteral;




#[test]
fn bool_true() -> Error  {
    validate(
        "let x = true;",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_true()
            )
        ])
    )
}

#[test]
fn bool_false() -> Error  {
    validate(
        "let x = false;",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_false()
            )
        ])
    )
}

#[test]
fn none() -> Error {
    validate(
        "let x = none;",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_none()
            )
        ])
    )
}

#[test]
fn int_zero() -> Error {
    validate(
        "let x = 0;",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_i(0)
            )
        ])
    )
}
#[test]
fn int_hex_ffff() -> Error {
    validate(
        "let x = 0xffff;",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_i("65535")
            )
        ])
    )
}

#[test]
fn int_large() -> Error {
    validate(
        "let x = 1234567890_1234567890_1234567890_1234567890_1234567890_1234567890;",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_i("123456789012345678901234567890123456789012345678901234567890")
            )
        ])
    )
}

#[test]
fn string_literal() -> Error {
    validate(
        "let x = \"this is a \\\"string\\\" literal!\";",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_string_literal("this is a \"string\" literal!".to_string())
            )
        ])
    )
}

#[test]
fn char_literal() -> Error {
    validate(
        "let x = '\\n';",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_i('\n' as u8)
            )
        ])
    )
}