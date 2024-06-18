use crate::ast::DeclarationFlags;
use crate::field_map;
use crate::tests::{Error, v_empty_def, v_i, validate};
use crate::validation::{AstValidator, v_bool, v_enum, v_enum_member, v_name_reference, v_program, v_signed_integer_type, v_structure, v_true, v_type_declaration, v_variable_declaration, v_field};

fn v_type_decl(alias: Box<AstValidator>) -> Box<AstValidator> {
    v_program(vec![
        v_type_declaration(
            DeclarationFlags::empty(),
            "T".to_string(),
            None,
            alias
        )
    ])
}

#[test]
fn tuple() -> Error  {
    validate(
        "type T is struct(bool,bool);",
        v_type_decl(v_structure(true,vec![],vec![
            v_bool(),
            v_bool()
        ]))
    )
}

#[test]
fn empty_structure() -> Error {
    validate(
        "type T is struct;",
        v_type_decl(v_structure(false,vec![],vec![]))
    )
}
#[test]
fn enumeration() -> Error {
    validate(
        "type T is enum{E=0,I(i32),B{x:i32,y:i32}};",
        v_type_decl(
            v_enum(None,vec![
                v_enum_member(
                    "E".to_string(),
                    false,
                    vec![],
                    Some(v_i(0))
                ),
                v_enum_member(
                    "I".to_string(),
                    true,
                    vec![v_signed_integer_type(32)],
                    None
                ),
                v_enum_member(
                    "B".to_string(),
                    false,
                    field_map![
                        "x" => v_signed_integer_type(32),
                        "y" => v_signed_integer_type(32)
                    ],
                    None
                )
            ])
        )
    )
}