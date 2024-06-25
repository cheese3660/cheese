use crate::ast::DeclarationFlags;
use crate::object_map;
use crate::tests::{Error, v_empty_def, v_f, v_i, validate};
use crate::ast::{v_array_call, v_array_literal, v_bool, v_enum_literal, v_false, v_field_literal, v_function_type, v_name, v_name_reference, v_object_call, v_object_literal, v_program, v_single, v_string, v_true, v_tuple_call, v_tuple_literal, v_typeof, v_variable_declaration, v_void};
#[test]
fn explicit_structure_literal() -> Error  {
    validate(
        "let x = p{a:false,b:false};",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_object_call(
                    v_name_reference("p".to_string()),
                    object_map![
                        "a" => v_false(),
                        "b" => v_false(),
                    ]
                )
            )
        ])
    )
}

#[test]
fn explicit_tuple_literal() -> Error {
    validate(
        "let x = p(false, false);",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_tuple_call(
                    v_name("p"),
                    vec![v_false(),v_false()]
                )
            )
        )
    )
}
#[test]
fn implicit_tuple_literal() -> Error {
    validate(
        "let x = .(false, false);",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_tuple_literal(
                    vec![v_false(),v_false()]
                )
            )
        )
    )
}

#[test]
fn implicit_structure_literal() -> Error {
    validate(
        "let x = .{a:false,b:false};",
        v_program(vec![
            v_variable_declaration(
                v_empty_def(),
                v_object_literal(
                    object_map![
                        "a" => v_false(),
                        "b" => v_false(),
                    ]
                )
            )
        ])
    )
}

#[test]
fn enumeration_constant() -> Error {
    validate(
        "let x = .E;",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_enum_literal("E".to_string())
            )
        )
    )
}

#[test]
fn explicit_array_literal() -> Error {
    validate(
        "let x = bool[false,true];",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_array_call(
                    v_bool(),
                    vec![
                        v_false(),
                        v_true()
                    ]
                )
            )
        )
    )
}

#[test]
fn implicit_array_literal() -> Error {
    validate(
        "let x = .[false,true];",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_array_literal(vec![
                    v_false(),
                    v_true()
                ])
            )
        )
    )
}

#[test]
fn typeof_type() -> Error {
    validate(
        "let x = typeof fn()->void;",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_typeof(
                    v_function_type(
                        DeclarationFlags::empty(),
                        vec![],
                        v_void()
                    )
                )
            )
        )
    )
}