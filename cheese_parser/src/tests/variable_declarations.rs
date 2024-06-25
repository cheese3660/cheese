use std::collections::HashMap;
use crate::ast::DeclarationFlags;
use crate::tests::{Error, v_empty_def, validate};
use crate::v_map;
use crate::ast::{v_destructure, v_name_reference, v_program, v_structure_destructure, v_true, v_tuple_destructure, v_underscore, v_variable_declaration, v_variable_definition};

#[test]
fn mutable_declaration() -> Error  {
    validate(
        "let x mut = true;",
        v_program(vec![
            v_variable_declaration(
                v_variable_definition(
                    DeclarationFlags::mutable,
                    "x".to_string(),
                    None
                ),
                v_true()
            )
        ])
    )
}

#[test]
fn destructuring() -> Error {
    validate(
        "let{a: (x mut, y, z),b:w,c:_} = a;",
        v_program(vec![
            v_destructure(
                v_structure_destructure(
                    v_map![
                        "a" => v_tuple_destructure(vec![
                            v_variable_definition(DeclarationFlags::mutable,"x".to_string(),None),
                            v_variable_definition(DeclarationFlags::empty(),"y".to_string(),None),
                            v_variable_definition(DeclarationFlags::empty(),"z".to_string(),None),
                        ]),
                        "b" => v_variable_definition(
                            DeclarationFlags::empty(),
                            "w".to_string(),
                            None
                        ),
                        "c" => v_underscore()
                    ]
                ),
                v_name_reference("a".to_string())
            )
        ])
    )
}