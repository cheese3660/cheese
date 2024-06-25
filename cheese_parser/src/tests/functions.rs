use crate::args;
use crate::ast::{AstNode, DeclarationFlags, NodeList, NodePtr, OptionalNode};
use crate::tests::{Error, validate};
use crate::ast::{v_addition, v_block, v_function, v_function_import, v_implicit_result, v_name, v_none, v_signed_integer_type, v_single, v_string, v_structure, v_tuple_literal, v_type, v_void, v_argument};


fn v_single_function<T: ToString>(declaration_flags: DeclarationFlags, name: T, generics: Option<NodeList>, args: NodeList, return_type: OptionalNode, body: NodePtr) -> NodePtr {
    v_single(
        v_function(
            declaration_flags,
            name.to_string(),
            generics,
            args,
            return_type,
            body
        )
    )
}

#[test]
fn no_argument_implicit_void_return_type_no_modifiers() -> Error {
    validate(
        "fn a() => none;",
        v_single_function(
            DeclarationFlags::empty(),
            "a",
            None,
            vec![],
            None,
            v_none()
        )
    )
}

#[test]
fn no_argument_explicit_void_return_type_no_modifiers() -> Error {
    validate(
        "fn a() -> void => none;",
        v_single_function(
            DeclarationFlags::empty(),
            "a",
            None,
            vec![],
            Some(v_void()),
            v_none()
        )
    )
}

#[test]
fn no_generic_single_argument_no_modifiers() -> Error {
    validate(
        "fn a(b: i32) -> i32 => b;",
        v_single_function(
            DeclarationFlags::empty(),
            "a",
            None,
            args![
                "b" => v_signed_integer_type(32)
            ],
            Some(v_signed_integer_type(32)),
            v_name("b")
        )
    )
}

#[test]
fn generic_single_argument_no_modifiers() -> Error {
    validate(
        "fn a<T: type>(b: T) -> T => b;",
        v_single_function(
            DeclarationFlags::empty(),
            "a",
            Some(args![
                "T" => v_type()
            ]),
            args![
                "b" => v_name("T")
            ],
            Some(v_name("T")),
            v_name("b")
        )
    )
}

#[test]
fn no_generic_multi_argument_no_modifiers() -> Error {
    validate(
        "fn a(b: i32,c:i32) -> i32 => b+c;",
        v_single_function(
            DeclarationFlags::empty(),
            "a",
            None,
            args![
                "b" => v_signed_integer_type(32),
                "c" => v_signed_integer_type(32),
            ],
            Some(v_signed_integer_type(32)),
            v_addition(
                v_name("b"),
                v_name("c")
            )
        )
    )
}

#[test]
fn generic_multi_argument_no_modifiers() -> Error {
    validate(
        "fn a<U: type, V: type>(b: U, c: V) -> struct(U,V) => .(b,c);",
        v_single_function(
            DeclarationFlags::empty(),
            "a",
            Some(args![
                "U" => v_type(),
                "V" => v_type()
            ]),
            args![
                "b" => v_name("U"),
                "c" => v_name("V")
            ],
            Some(v_structure(
                true,
                vec![],
                vec![
                    v_name("U"),
                    v_name("V")
                ]
            )),
            v_tuple_literal(
                vec![
                    v_name("b"),
                    v_name("c")
                ]
            )
        )
    )
}

#[test]
fn multi_argument_no_modifiers_block() -> Error {
    validate(
        "fn a(b: i32,c:i32) -> i32 { b+c }",
        v_single_function(
            DeclarationFlags::empty(),
            "a",
            None,
            args![
                "b" => v_signed_integer_type(32),
                "c" => v_signed_integer_type(32),
            ],
            Some(v_signed_integer_type(32)),
            v_block(vec![
                v_implicit_result(
                    v_addition(
                        v_name("b"),
                        v_name("c")
                    )
                )
            ])
        )
    )
}

#[test]
fn no_argument_inline_modifier() -> Error {
    validate(
        "fn a() inline => none;",
        v_single_function(
            DeclarationFlags::inline,
            "a",
            None,
            vec![],
            None,
            v_none()
        )
    )
}

#[test]
fn no_argument_extern_modifier() -> Error {
    validate(
        "fn a() extern -> void => none;",
        v_single_function(
            DeclarationFlags::external,
            "a",
            None,
            vec![],
            Some(v_void()),
            v_none()
        )
    )
}

#[test]
fn no_argument_export_modifier() -> Error {
    validate(
        "fn a() export => none;",
        v_single_function(
            DeclarationFlags::export,
            "a",
            None,
            vec![],
            None,
            v_none()
        )
    )
}

#[test]
fn no_argument_comptime_modifier() -> Error {
    validate(
        "fn a() comptime => none;",
        v_single_function(
            DeclarationFlags::comptime,
            "a",
            None,
            vec![],
            None,
            v_none()
        )
    )
}

#[test]
fn no_argument_public_modifier() -> Error {
    validate(
        "fn a() public => none;",
        v_single_function(
            DeclarationFlags::public,
            "a",
            None,
            vec![],
            None,
            v_none()
        )
    )
}

#[test]
fn no_argument_private_modifier() -> Error {
    validate(
        "fn a() private => none;",
        v_single_function(
            DeclarationFlags::private,
            "a",
            None,
            vec![],
            None,
            v_none()
        )
    )
}

#[test]
fn no_argument_entry_modifier() -> Error {
    validate(
        "fn a() entry => none;",
        v_single_function(
            DeclarationFlags::entry,
            "a",
            None,
            vec![],
            None,
            v_none()
        )
    )
}


#[test]
fn no_argument_multiple_modifiers() -> Error {
    validate(
        "fn a() export entry => none;",
        v_single_function(
            DeclarationFlags::export | DeclarationFlags::entry,
            "a",
            None,
            vec![],
            None,
            v_none()
        )
    )
}

#[test]
fn importing_no_library() -> Error {
    validate(
        "fn a() -> void import;",
        v_single(
            v_function_import(
                DeclarationFlags::empty(),
                "a".to_string(),
                vec![],
                Some(v_void()),
                None
            )
        )
    )
}


#[test]
fn importing_library() -> Error {
    validate(
        "fn a() import(\"test.dll\");",
        v_single(
            v_function_import(
                DeclarationFlags::empty(),
                "a".to_string(),
                vec![],
                None,
                Some(v_string("test.dll"))
            )
        )
    )
}