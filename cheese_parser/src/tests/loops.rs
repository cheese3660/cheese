use crate::{args, v_map};
use crate::ast::DeclarationFlags;
use crate::tests::{Error, v_empty_static, v_f, v_i, validate};
use std::collections::HashMap;
use crate::ast::{v_closure, v_const_reference_implicit_capture, v_none, v_single, v_variable_declaration, v_argument, v_bool, v_reference_capture, v_reference, v_constant_reference_capture, v_copy_capture, v_reference_implicit_capture, v_copy_implicit_capture, v_void, v_function, v_signed_integer_type, v_if, v_lesser_than, v_name, v_unary_minus, v_match, v_match_arm, v_match_value, v_match_range, v_match_constraint, v_match_all, v_destructuring_match_array, v_destructuring_match_structure, v_destructuring_match_arm, v_destructuring_match_tuple, v_enum_literal, v_match_enum_tuple, v_match_enum_structure, v_no_return, v_loop, v_false, v_break, v_string, v_while, v_true, v_continue};

#[test]
fn loop_no_yield() -> Error {
    validate(
        "fn x() -> noreturn => loop false;",
        v_single(
            v_function(
                DeclarationFlags::empty(),
                "x".to_string(),
                None,
                vec![],
                Some(v_no_return()),
                v_loop(
                    v_false()
                )
            )
        )
    )
}

#[test]
fn loop_yield() -> Error {
    validate(
        "fn x() -> bool => loop break false;",
        v_single(
            v_function(
                DeclarationFlags::empty(),
                "x".to_string(),
                None,
                vec![],
                Some(v_bool()),
                v_loop(
                    v_break(
                        v_false()
                    )
                )
            )
        )
    )
}

#[test]
fn while_loop() -> Error {
    validate(
        "fn x() -> noreturn => while (true) false;",
        v_single(
            v_function(
                DeclarationFlags::empty(),
                "x".to_string(),
                None,
                vec![],
                Some(v_no_return()),
                v_while(
                    v_true(),
                    v_false(),
                    None
                )
            )
        )
    )
}

#[test]
fn while_loop_else() -> Error {
    validate(
        "fn x() -> bool => while (false) false else true;",
        v_single(
            v_function(
                DeclarationFlags::empty(),
                "x".to_string(),
                None,
                vec![],
                Some(v_bool()),
                v_while(
                    v_false(),
                    v_false(),
                    Some(v_true())
                )
            )
        )
    )
}

#[test]
fn while_loop_continue() -> Error {
    validate(
        "fn x() -> noreturn => while (true) continue;",
        v_single(
            v_function(
                DeclarationFlags::empty(),
                "x".to_string(),
                None,
                vec![],
                Some(v_no_return()),
                v_while(
                    v_true(),
                    v_continue(),
                    None
                )
            )
        )
    )
}
