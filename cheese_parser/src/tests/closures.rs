use crate::args;
use crate::tests::{Error, v_empty_def, validate};
use crate::validation::{AstValidator, v_closure, v_const_reference_implicit_capture, v_none, v_single, v_variable_declaration, v_argument, v_bool, v_reference_capture, v_reference, v_constant_reference_capture, v_copy_capture, v_reference_implicit_capture, v_copy_implicit_capture, v_void};

fn v_single_closure(arguments: Vec<Box<AstValidator>>, captures: Vec<Box<AstValidator>>, return_type: Option<Box<AstValidator>>, body: Box<AstValidator>) -> Box<AstValidator> {
    v_single(
        v_variable_declaration(
            v_empty_def(),
            v_closure(
                arguments,
                captures,
                return_type,
                body
            )
        )
    )
}

#[test]
fn no_arguments_implicit_captures_no_return_type() -> Error {
    validate(
        "let x = || none;",
        v_single_closure(
            vec![],
            vec![
                v_const_reference_implicit_capture()
            ],
            None,
            v_none()
        )
    )
}

#[test]
fn one_argument_implicit_captures_no_return_type() -> Error {
    validate(
        "let x = |y:bool| none;",
        v_single_closure(
            args![
                "y" => v_bool()
            ],
            vec![
                v_const_reference_implicit_capture()
            ],
            None,
            v_none()
        )
    )
}

#[test]
fn multiple_arguments_implicit_captures_no_return_type() -> Error {
    validate(
        "let x = |y:bool,z:bool| none;",
        v_single_closure(
            args![
                "y" => v_bool(),
                "z" => v_bool()
            ],
            vec![
                v_const_reference_implicit_capture()
            ],
            None,
            v_none()
        )
    )
}

macro_rules! capture {
    (* $name:ident) => {
        v_reference_capture(stringify!($name).to_string())
    };
    (~ $name:ident) => {
        v_constant_reference_capture(stringify!($name).to_string())
    };
    (= $name:ident) => {
        v_copy_capture(stringify!($name).to_string())
    };
}

macro_rules! captures_const {
    [$($tok:tt $name:ident),*$(,)?] => {
        vec![
            $(
                capture!($tok $name),
            )*
            v_const_reference_implicit_capture(),
        ]
    };
}

macro_rules! captures {
    [$($tok:tt $name:ident),*$(,)?] => {
        vec![
            $(
                capture!($tok $name),
            )*
        ]
    };
}


#[test]
fn no_arguments_no_captures_no_return_type() -> Error {
    validate(
        "let x = ||[] none;",
        v_single_closure(
            vec![],
            vec![],
            None,
            v_none()
        )
    )
}

#[test]
fn no_arguments_implicit_const_reference_capture_no_return_type() -> Error {
    validate(
        "let x = ||[*~] none;",
        v_single_closure(
            vec![],
            vec![v_const_reference_implicit_capture()],
            None,
            v_none()
        )
    )
}
#[test]
fn no_arguments_implicit_reference_capture_no_return_type() -> Error {
    validate(
        "let x = ||[*] none;",
        v_single_closure(
            vec![],
            vec![v_reference_implicit_capture()],
            None,
            v_none()
        )
    )
}
#[test]
fn no_arguments_implicit_copy_capture_no_return_type() -> Error {
    validate(
        "let x = ||[=] none;",
        v_single_closure(
            vec![],
            vec![v_copy_implicit_capture()],
            None,
            v_none()
        )
    )
}

#[test]
fn no_arguments_explicit_constant_reference_capture_no_return_type() -> Error {
    validate(
        "let x = ||[*~y] none;",
        v_single_closure(
            vec![],
            captures![~y],
                None,
            v_none()
        )
    )
}

#[test]
fn no_arguments_explicit_reference_capture_no_return_type() -> Error {
    validate(
        "let x = ||[*y] none;",
        v_single_closure(
            vec![],
            captures![*y],
            None,
            v_none()
        )
    )
}

#[test]
fn no_arguments_explicit_copy_capture_no_return_type() -> Error {
    validate(
        "let x = ||[=y] none;",
        v_single_closure(
            vec![],
            captures![=y],
            None,
            v_none()
        )
    )
}

#[test]
fn no_arguments_multiple_captures_no_return_type() -> Error {
    validate(
        "let x = ||[*a,=b,*~c,*~] none;",
        v_single_closure(
            vec![],
            captures_const![
                *a,
                =b,
                ~c
            ],
            None,
            v_none()
        )
    )
}

#[test]
fn no_arguments_implicit_captures_return_type() -> Error {
    validate(
        "let x = ||->void none;",
        v_single_closure(
            vec![],
            vec![v_const_reference_implicit_capture()],
            Some(v_void()),
            v_none()
        )
    )
}