use crate::object_map;
use crate::tests::{Error, v_empty_def, v_i, validate};
use crate::validation::{v_enum_literal, v_false, v_field_literal, v_name, v_name_reference, v_object_call, v_object_literal, v_program, v_single, v_true, v_tuple_call, v_tuple_literal, v_variable_declaration};
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