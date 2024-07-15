use crate::ast::{DeclarationFlags, v_false, v_imaginary_literal, v_none, v_static_variable_declaration};
use crate::tests::{Error, v_f, v_i, validate};
use crate::ast::{v_bool, v_compile_time_complex, v_compile_time_float, v_compile_time_integer, v_compile_time_string, v_complex32, v_complex64, v_float32, v_float64, v_no_return, v_opaque, v_program, v_signed_integer_type, v_type, v_unsigned_integer_type, v_variable_definition, v_void};

#[test]
fn bool() -> Error  {
    validate(
        "let x: bool = false;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_bool()),
                v_false()
            )
        ])
    )
}

#[test]
fn i1() -> Error  {
    validate(
        "let x: i1 = 1;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_signed_integer_type(1)),
                v_i(1)
            )
        ])
    )
}

#[test]
fn i64() -> Error  {
    validate(
        "let x: i64 = 322;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_signed_integer_type(64)),
                v_i(322)
            )
        ])
    )
}

#[test]
fn i65535() -> Error {
    validate(
        "let x: i65535 = 12345678901234567890;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_signed_integer_type(65535)),
                v_i("12345678901234567890")
            )
        ])
    )
}

#[test]
fn u1() -> Error  {
    validate(
        "let x: u1 = 1;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_unsigned_integer_type(1)),
                v_i(1)
            )
        ])
    )
}

#[test]
fn u64() -> Error  {
    validate(
        "let x: u64;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_unsigned_integer_type(64)),
                v_i(1)
            )
        ])
    )
}

#[test]
fn u65535() -> Error {
    validate(
        "let x: u65535 = 1;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_unsigned_integer_type(65535)),
                v_i(1)
            )
        ])
    )
}

#[test]
fn f32() -> Error {
    validate(
        "let x: f32 = 0.0;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_float32()),
                v_f(0.0)
            )
        ])
    )
}

#[test]
fn f64() -> Error {
    validate(
        "let x: f64 = 0.0;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_float64()),
                v_f(0.0)
            )
        ])
    )
}

#[test]
fn c32() -> Error {
    validate(
        "let x: c32 = 0.0I;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_complex32()),
                v_imaginary_literal(0.0)
            )
        ])
    )
}

#[test]
fn c64() -> Error {
    validate(
        "let x: c64 = 0.0I;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_complex64()),
                v_imaginary_literal(0.0)
            )
        ])
    )
}
#[test]
fn opaque() -> Error {
    validate(
        "let x: opaque = none;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_opaque()),
                v_none()
            )
        ])
    )
}
#[test]
fn void() -> Error {
    validate(
        "let x: void = none;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_void()),
                v_none()
            )
        ])
    )
}

#[test]
fn comptime_float() -> Error {
    validate(
        "let x: comptime_float = 0.0;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_compile_time_float()),
                v_f(0.0)
            )
        ])
    )
}


#[test]
fn comptime_complex() -> Error {
    validate(
        "let x: comptime_complex = 0.0I;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_compile_time_complex()),
                v_imaginary_literal(0.0)
            )
        ])
    )
}

#[test]
fn comptime_int() -> Error {
    validate(
        "let x: comptime_int = none;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_compile_time_integer()),
                v_none()
            )
        ])
    )
}

#[test]
fn comptime_string() -> Error {
    validate(
        "let x: comptime_string = none;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_compile_time_string()),
                v_none()
            )
        ])
    )
}

#[test]
fn noreturn() -> Error {
    validate(
        "let x: noreturn = none;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_no_return()),
                v_none()
            )
        ])
    )
}
#[test]
fn type_type() -> Error {
    validate(
        "let x: type = none;",
        v_program(vec![
            v_static_variable_declaration(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_type()),
                v_none()
            )
        ])
    )
}