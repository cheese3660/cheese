use crate::{args, v_map};
use crate::ast::DeclarationFlags;
use crate::tests::{Error, v_empty_def, v_f, v_i, validate};
use std::collections::HashMap;
use crate::ast::{v_closure, v_const_reference_implicit_capture, v_none, v_single, v_variable_declaration, v_argument, v_bool, v_reference_capture, v_reference, v_constant_reference_capture, v_copy_capture, v_reference_implicit_capture, v_copy_implicit_capture, v_void, v_function, v_signed_integer_type, v_if, v_lesser_than, v_name, v_unary_minus, v_match, v_match_arm, v_match_value, v_match_range, v_match_constraint, v_match_all, v_destructuring_match_array, v_destructuring_match_structure, v_destructuring_match_arm, v_destructuring_match_tuple, v_enum_literal, v_match_enum_tuple, v_match_enum_structure, v_no_return, v_loop, v_false, v_break, v_string, v_while, v_true, v_continue, v_for, v_filter_transformation, v_map_transformation};


#[test]
fn copy() -> Error {
    validate(
        "fn x() -> noreturn => for (y : z) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn reference() -> Error {
    validate(
        "fn x() -> noreturn => for (*y : z) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_reference_capture("y".to_string()),
                None,
                v_name("z"),
                vec![],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn constant_reference() -> Error {
    validate(
        "fn x() -> noreturn => for (*~y : z) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_constant_reference_capture("y".to_string()),
                None,
                v_name("z"),
                vec![],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn index() -> Error {
    validate(
        "fn x() -> noreturn => for (y,i : z) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                Some(v_name("i")),
                v_name("z"),
                vec![],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn single_filter() -> Error {
    validate(
        "fn x() -> noreturn => for (y : z ? is_prime) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![v_filter_transformation(v_name("is_prime"))],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn multiple_filter() -> Error {
    validate(
        "fn x() -> noreturn => for (y : z ? is_prime ? greater_than_ten) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![v_filter_transformation(v_name("is_prime")),v_filter_transformation(v_name("greater_than_ten"))],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn single_map() -> Error {
    validate(
        "fn x() -> noreturn => for (y : z : square) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![v_map_transformation(v_name("square"))],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn multiple_maps() -> Error {
    validate(
        "fn x() -> noreturn => for (y : z : square : add_one) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![v_map_transformation(v_name("square")),v_map_transformation(v_name("add_one"))],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn single_filter_single_map() -> Error {
    validate(
        "fn x() -> noreturn => for (y : z ? is_prime : square) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![v_filter_transformation(v_name("is_prime")),v_map_transformation(v_name("square"))],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn multiple_filters_single_map() -> Error {
    validate(
        "fn x() -> noreturn => for (y : z ? is_prime ? greater_than_ten : square) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![v_filter_transformation(v_name("is_prime")),v_filter_transformation(v_name("greater_than_ten")),v_map_transformation(v_name("square"))],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn single_filter_multiple_maps() -> Error {
    validate(
        "fn x() -> noreturn => for (y : z ? is_prime : square : add_one) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![v_filter_transformation(v_name("is_prime")),v_map_transformation(v_name("square")),v_map_transformation(v_name("add_one"))],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn multiple_filters_multiple_maps() -> Error {
    validate(
        "fn x() -> noreturn => for (y : z ? is_prime ? greater_than_ten : square : add_one) false;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_no_return()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![v_filter_transformation(v_name("is_prime")),v_filter_transformation(v_name("greater_than_ten")),v_map_transformation(v_name("square")),v_map_transformation(v_name("add_one"))],
                v_false(),
                None
            )
        ))
    )
}

#[test]
fn for_else() -> Error {
    validate(
        "fn x() -> bool => for (y : z) false else true;",
        v_single(v_function(
            DeclarationFlags::empty(),
            "x".to_string(),
            None,
            vec![],
            Some(v_bool()),
            v_for(
                v_copy_capture("y".to_string()),
                None,
                v_name("z"),
                vec![],
                v_false(),
                Some(v_true())
            )
        ))
    )
}