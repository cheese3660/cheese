use crate::ast::DeclarationFlags;
use crate::tests::{Error, validate};
use crate::validation::{v_bool, v_compile_time_complex, v_compile_time_float, v_compile_time_integer, v_compile_time_string, v_complex32, v_complex64, v_float32, v_float64, v_no_return, v_opaque, v_program, v_signed_integer_type, v_type, v_unsigned_integer_type, v_variable_definition, v_void};

#[test]
fn bool() -> Error  {
    validate(
        "def x: bool;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_bool())
            )
        ])
    )
}

#[test]
fn i1() -> Error  {
    validate(
        "def x: i1;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_signed_integer_type(1))
            )
        ])
    )
}

#[test]
fn i64() -> Error  {
    validate(
        "def x: i64;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_signed_integer_type(64))
            )
        ])
    )
}

#[test]
fn i65535() -> Error {
    validate(
        "def x: i65535;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_signed_integer_type(65535))
            )
        ])
    )
}

#[test]
fn u1() -> Error  {
    validate(
        "def x: u1;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_unsigned_integer_type(1))
            )
        ])
    )
}

#[test]
fn u64() -> Error  {
    validate(
        "def x: u64;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_unsigned_integer_type(64))
            )
        ])
    )
}

#[test]
fn u65535() -> Error {
    validate(
        "def x: u65535;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_unsigned_integer_type(65535))
            )
        ])
    )
}

#[test]
fn f32() -> Error {
    validate(
        "def x: f32;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_float32())
            )
        ])
    )
}

#[test]
fn f64() -> Error {
    validate(
        "def x: f64;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_float64())
            )
        ])
    )
}

#[test]
fn c32() -> Error {
    validate(
        "def x: c32;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_complex32())
            )
        ])
    )
}

#[test]
fn c64() -> Error {
    validate(
        "def x: c64;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_complex64())
            )
        ])
    )
}
#[test]
fn opaque() -> Error {
    validate(
        "def x: opaque;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_opaque())
            )
        ])
    )
}
#[test]
fn void() -> Error {
    validate(
        "def x: void;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_void())
            )
        ])
    )
}

#[test]
fn comptime_float() -> Error {
    validate(
        "def x: comptime_float;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_compile_time_float())
            )
        ])
    )
}


#[test]
fn comptime_complex() -> Error {
    validate(
        "def x: comptime_complex;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_compile_time_complex())
            )
        ])
    )
}

#[test]
fn comptime_int() -> Error {
    validate(
        "def x: comptime_int;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_compile_time_integer())
            )
        ])
    )
}

#[test]
fn comptime_string() -> Error {
    validate(
        "def x: comptime_string;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_compile_time_string())
            )
        ])
    )
}

#[test]
fn noreturn() -> Error {
    validate(
        "def x: noreturn;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_no_return())
            )
        ])
    )
}
#[test]
fn type_type() -> Error {
    validate(
        "def x: type;",
        v_program(vec![
            v_variable_definition(
                DeclarationFlags::empty(),
                "x".to_string(),
                Some(v_type())
            )
        ])
    )
}