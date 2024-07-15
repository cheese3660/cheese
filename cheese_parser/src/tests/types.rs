use crate::ast::{DeclarationFlags, NodePtr, v_single};
use crate::{field_map, object_map};
use crate::tests::{Error, v_empty_static, v_i, validate};
use crate::ast::{v_bool, v_enum, v_enum_member, v_name_reference, v_program, v_signed_integer_type, v_structure, v_true, v_type_declaration, v_variable_declaration, v_field, v_reference, v_array_type, v_unknown_size, v_slice, v_function_type, v_void, v_name, v_interface};

fn v_type_decl(alias: NodePtr) -> NodePtr {
    v_type_declaration(DeclarationFlags::empty(), "T".to_string(), None, alias)
}

#[test]
fn tuple() -> Error  {
    validate(
        "struct T(bool,bool);",
        v_single(v_structure(DeclarationFlags::empty(),"T".to_string(),false,None,vec![
            v_bool(),
            v_bool()
        ]))
    )
}

#[test]
fn empty_structure() -> Error {
    validate(
        "struct T;",
        v_single(v_structure(DeclarationFlags::empty(),"T".to_string(),false,None,vec![]))
    )
}
#[test]
fn enumeration() -> Error {
    validate(
        "enum T{E=0,I(i32),B{x:i32,y:i32}};",
        v_single(
            v_enum(DeclarationFlags::empty(),"T".to_string(),None,None,vec![
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

#[test]
fn mutable_reference() -> Error {
    validate(
        "type T is *bool;",
        v_type_decl(v_reference(false,v_bool()))
    )
}

#[test]
fn immutable_reference() -> Error {
    validate(
        "type T is *~bool;",
        v_type_decl(v_reference(true,v_bool()))
    )
}

#[test]
fn known_single_dimension_mutable_array() -> Error {
    validate(
        "type T is [1]bool;",
        v_type_decl(v_array_type(false,vec![v_i(1)],v_bool()))
    )
}
#[test]
fn known_single_dimension_immutable_array() -> Error {
    validate(
        "type T is [1]~bool;",
        v_type_decl(v_array_type(true,vec![v_i(1)],v_bool()))
    )
}
#[test]
fn known_multi_dimension_mutable_array() -> Error {
    validate(
        "type T is [1,1]bool;",
        v_type_decl(v_array_type(false,vec![v_i(1),v_i(1)],v_bool()))
    )
}

#[test]
fn known_multi_dimension_immutable_array() -> Error {
    validate(
        "type T is [1,1]~bool;",
        v_type_decl(v_array_type(true,vec![v_i(1),v_i(1)],v_bool()))
    )
}
#[test]
fn unknown_single_dimension_mutable_array() -> Error {
    validate(
        "type T is [?]bool;",
        v_type_decl(v_array_type(false,vec![v_unknown_size()],v_bool()))
    )
}
#[test]
fn unknown_single_dimension_immutable_array() -> Error {
    validate(
        "type T is [?]~bool;",
        v_type_decl(v_array_type(true,vec![v_unknown_size()],v_bool()))
    )
}

#[test]
fn unknown_multi_dimension_mutable_array() -> Error {
    validate(
        "type T is [1,?]bool;",
        v_type_decl(v_array_type(false,vec![v_i(1),v_unknown_size()],v_bool()))
    )
}
#[test]
fn unknown_multi_dimension_immutable_array() -> Error {
    validate(
        "type T is [1,?]~bool;",
        v_type_decl(v_array_type(true,vec![v_i(1),v_unknown_size()],v_bool()))
    )
}
#[test]
fn mutable_slice() -> Error {
    validate(
        "type T is <>bool;",
        v_type_decl(v_slice(false,v_bool()))
    )
}
#[test]
fn immutable_slice() -> Error {
    validate(
        "type T is <>~bool;",
        v_type_decl(v_slice(true,v_bool()))
    )
}

#[test]
fn function_no_arguments() -> Error {
    validate(
        "type T is fn()->void;",
        v_type_decl(v_function_type(
            DeclarationFlags::empty(),
            vec![],
            v_void()
        ))
    )
}

#[test]
fn function_one_argument() -> Error {
    validate(
        "type T is fn(bool)->void;",
        v_type_decl(v_function_type(
            DeclarationFlags::empty(),
            vec![
                v_bool(),
            ],
            v_void()
        ))
    )
}

#[test]
fn function_multiple_arguments() -> Error {
    validate(
        "type T is fn(bool,bool)->void;",
        v_type_decl(v_function_type(
            DeclarationFlags::empty(),
            vec![
                v_bool(),
                v_bool()
            ],
            v_void()
        ))
    )
}

#[test]
fn structure_no_children_no_interfaces() -> Error {
    validate(
        "struct T{};",
        v_single(v_structure(
            DeclarationFlags::empty(),
            "T".to_string(),
            false,
            None,
            field_map!{
            }
        ))
    )
}

#[test]
fn structure_single_child_no_interfaces() -> Error {
    validate(
        "struct T{a:bool};",
        v_single(v_structure(
            DeclarationFlags::empty(),
            "T".to_string(),
            false,
            None,
            field_map!{
                "a" => v_bool(),
            }
        ))
    )
}

#[test]
fn structure_multiple_children_no_interfaces() -> Error {
    validate(
        "struct T{a:bool,b:bool};",
        v_single(v_structure(
            DeclarationFlags::empty(),
            "T".to_string(),
            false,
            None,
            field_map!{
                "a" => v_bool(),
                "b" => v_bool()
            }
        ))
    )
}

#[test]
fn tuple_no_children() -> Error {
    validate(
        "struct T();",
        v_single(v_structure(
            DeclarationFlags::empty(),
            "T".to_string(),
            true,
            None,
            vec![]
        ))
    )
}
#[test]
fn tuple_single_child() -> Error {
    validate(
        "struct T(bool);",
        v_single(v_structure(
            DeclarationFlags::empty(),
            "T".to_string(),
            true,
            None,
            vec![v_bool()]
        ))
    )
}
#[test]
fn tuple_multiple_children() -> Error {
    validate(
        "struct T(bool,bool);",
        v_single(v_structure(
            DeclarationFlags::empty(),
            "T".to_string(),
            true,
            None,
            vec![v_bool(),v_bool()]
        ))
    )
}

#[test]
fn interface_static_no_interfaces() -> Error {
    validate(
        "interface T{}",
        v_single(v_interface(
            DeclarationFlags::empty(),
            "T".to_string(),
            None,
            vec![]
        ))
    )
}