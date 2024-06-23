use crate::args;
use crate::ast::DeclarationFlags;
use crate::tests::{Error, v_empty_def, v_i, validate};
use crate::validation::{AstValidator, v_closure, v_const_reference_implicit_capture, v_none, v_single, v_variable_declaration, v_argument, v_bool, v_reference_capture, v_reference, v_constant_reference_capture, v_copy_capture, v_reference_implicit_capture, v_copy_implicit_capture, v_void, v_function, v_signed_integer_type, v_if, v_lesser_than, v_name, v_unary_minus};

#[test]
fn if_expressions() -> Error {
    validate(
        "fn abs(x: i32) -> i32 => if (x < 0) -x else x;",
        v_single(
            v_function(
                DeclarationFlags::empty(),
                "abs".to_string(),
                None,
                args!["x" => v_signed_integer_type(32)],
                Some(v_signed_integer_type(32)),
                v_if(
                    v_lesser_than(
                        v_name("x"),
                        v_i(0)
                    ),
                    None,
                    v_unary_minus(
                        v_name("x")
                    ),
                    Some(v_name("x"))
                )
            )
        )
    )
}

#[test]
fn if_unwrapping_copy() -> Error {
    validate(
        "let x = if (y: z) z;",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_if(
                    v_name("y"),
                    Some(v_copy_capture("z".to_string())),
                    v_name("z"),
                    None
                )
            )
        )
    )
}

#[test]
fn if_unwrapping_ref() -> Error {
    validate(
        "let x = if (y: *z) z;",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_if(
                    v_name("y"),
                    Some(v_reference_capture("z".to_string())),
                    v_name("z"),
                    None
                )
            )
        )
    )
}

#[test]
fn if_unwrapping_constant_ref() -> Error {
    validate(
        "let x = if (y: *~z) z;",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_if(
                    v_name("y"),
                    Some(v_constant_reference_capture("z".to_string())),
                    v_name("z"),
                    None
                )
            )
        )
    )
}