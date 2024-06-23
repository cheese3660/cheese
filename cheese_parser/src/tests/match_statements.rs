use crate::{args, v_map};
use crate::ast::DeclarationFlags;
use crate::tests::{Error, v_empty_def, v_i, validate};
use std::collections::HashMap;
use crate::validation::{AstValidator, v_closure, v_const_reference_implicit_capture, v_none, v_single, v_variable_declaration, v_argument, v_bool, v_reference_capture, v_reference, v_constant_reference_capture, v_copy_capture, v_reference_implicit_capture, v_copy_implicit_capture, v_void, v_function, v_signed_integer_type, v_if, v_lesser_than, v_name, v_unary_minus, v_match, v_match_arm, v_match_value, v_match_range, v_match_constraint, v_match_all, v_destructuring_match_array, v_destructuring_match_structure, v_destructuring_match_arm, v_destructuring_match_tuple, v_enum_literal, v_match_enum_tuple, v_match_enum_structure};


fn v_match_y(arms: Vec<Box<AstValidator>>) -> Box<AstValidator> {
    v_single(v_variable_declaration(v_empty_def(),v_match(v_name("y"),arms)))
}

#[test]
fn value_constraints() -> Error {
    validate(
        "let x = match (y){0 => 0,\n1,2 => 1,\n3,4,5,6,7 => 2};",
        v_match_y(
            vec![
                v_match_arm(
                    vec![v_match_value(v_i(0))],
                    None,
                    v_i(0)
                ),
                v_match_arm(
                    vec![v_match_value(v_i(1)),v_match_value(v_i(2))],
                    None,
                    v_i(1)
                ),
                v_match_arm(
                    vec![v_match_value(v_i(3)),v_match_value(v_i(4)),v_match_value(v_i(5)),v_match_value(v_i(6)),v_match_value(v_i(7))],
                    None,
                    v_i(2)
                ),
            ]
        )
    )
}

#[test]
fn range_constraints() -> Error {
    validate(
        "let x = match (y){0 .. 7 => 0};",
        v_match_y(vec![
            v_match_arm(
                vec![v_match_range(v_i(0),v_i(7))],
                None,
                v_i(0)
            )
        ])
    )
}

#[test]
fn constrain() -> Error {
    validate(
        "let x = match (y){constrain is_even => 1, _ => 0};",
        v_match_y(
            vec![
                v_match_arm(
                    vec![v_match_constraint(v_name("is_even"))],
                    None,
                    v_i(1)
                ),
                v_match_arm(
                    vec![v_match_all()],
                    None,
                    v_i(0)
                )
            ]
        )
    )
}

#[test]
fn catchall_copy() -> Error {
    validate(
        "let x = match (y){_ -> z => z};",
        v_match_y(
            vec![
                v_match_arm(
                    vec![v_match_all()],
                    Some(v_copy_capture("z".to_string())),
                    v_name("z")
                )
            ]
        )
    )
}

#[test]
fn catchall_ref() -> Error {
    validate(
        "let x = match (y){_ -> *z => z};",
        v_match_y(
            vec![
                v_match_arm(
                    vec![v_match_all()],
                    Some(v_reference_capture("z".to_string())),
                    v_name("z")
                )
            ]
        )
    )
}

#[test]
fn catchall_constant_ref() -> Error {
    validate(
        "let x = match (y){_ -> *~z => z};",
        v_match_y(
            vec![
                v_match_arm(
                    vec![v_match_all()],
                    Some(v_constant_reference_capture("z".to_string())),
                    v_name("z")
                )
            ]
        )
    )
}

#[test]
fn destructuring_all_captures_all_constraints() -> Error {
    validate(
        "let x = match (y){.{a: .(_ -> i; _ -> *j; 0 .. 7 -> *~k); b: 1; c: constrain is_even} => 0};",
        v_match_y(
            vec![
                v_match_arm(
                    vec![
                        v_destructuring_match_structure(
                            v_map!{
                                "a" => v_destructuring_match_arm(
                                    vec![
                                        v_destructuring_match_tuple(
                                            vec![
                                                v_destructuring_match_arm(
                                                    vec![
                                                        v_match_all()
                                                    ],
                                                    Some(v_copy_capture("i".to_string()))
                                                ),
                                                v_destructuring_match_arm(
                                                    vec![
                                                        v_match_all()
                                                    ],
                                                    Some(v_reference_capture("j".to_string()))
                                                ),
                                                v_destructuring_match_arm(
                                                    vec![
                                                        v_match_range(v_i(0),v_i(7))
                                                    ],
                                                    Some(v_reference_capture("k".to_string()))
                                                ),
                                            ]
                                        )
                                    ],
                                    None
                                ),
                                "b" => v_destructuring_match_arm(
                                    vec![v_match_value(v_i(1))],
                                    None,
                                ),
                                "c" => v_destructuring_match_arm(
                                    vec![v_match_constraint(v_name("is_even"))],
                                    None
                                )
                            }
                        )
                    ],
                    None,
                    v_i(0)
                )
            ]
        )
    )
}

#[test]
fn enum_destructuring() -> Error {
    validate(
        "let x = match (n) {.empty => 0, .integer(0 .. 255 -> small_int) => 1, .integer(_ => big_int) => 2, .add{lhs: constrain is_constant, rhs: constrain is_constant} => 3, .add{lhs: _; rhs: _} => 4, _ => 5};",
        v_match_y(
            vec![
                v_match_arm(
                    vec![
                        v_match_value(v_enum_literal("empty".to_string()))
                    ],
                    None,
                    v_i(0)
                ),
                v_match_arm(
                    vec![
                        v_match_enum_tuple(
                            "integer".to_string(),
                            vec![
                                v_destructuring_match_arm(
                                    vec![v_match_range(v_i(0),v_i(255))],
                                    Some(v_copy_capture("small_int".to_string()))
                                )
                            ]
                        )
                    ],
                    None,
                    v_i(1)
                ),
                v_match_arm(
                    vec![
                        v_match_enum_tuple(
                            "integer".to_string(),
                            vec![
                                v_destructuring_match_arm(
                                    vec![v_match_all()],
                                    Some(v_copy_capture("big_int".to_string()))
                                )
                            ]
                        )
                    ],
                    None,
                    v_i(2)
                ),
                v_match_arm(
                    vec![
                        v_match_enum_structure(
                            "add".to_string(),
                            v_map!{
                                "lhs" => v_destructuring_match_arm(vec![v_match_constraint(v_name("is_constant"))],None),
                                "rhs" => v_destructuring_match_arm(vec![v_match_constraint(v_name("is_constant"))],None),
                            }
                        )
                    ],
                    None,
                    v_i(3)
                ),
                v_match_arm(
                    vec![
                        v_match_enum_structure(
                            "add".to_string(),
                            v_map!{
                                "lhs" => v_destructuring_match_arm(vec![v_match_all()],None),
                                "rhs" => v_destructuring_match_arm(vec![v_match_all()],None),
                            }
                        )
                    ],
                    None,
                    v_i(4)
                ),
                v_match_arm(
                    vec![
                        v_match_all()
                    ],
                    None,
                    v_i(5)
                )
            ]
        )
    )
}