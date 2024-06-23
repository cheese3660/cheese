#[cfg(test)]
pub mod primitive_types;

#[cfg(test)]
pub mod primitive_values;

#[cfg(test)]
mod complex_values;

#[cfg(test)]
mod variable_declarations;

#[cfg(test)]
mod types;

#[cfg(test)]
mod functions;

#[cfg(test)]
mod closures;

#[cfg(test)]
mod if_expressions;

#[cfg(test)]
mod match_statements;

#[cfg(test)]
mod loops;

#[cfg(test)]
mod for_loops;

#[cfg(test)]
mod operators;

use std::str::FromStr;
use num_bigint::BigInt;
use cheese_diagnostics::locating::File;
use cheese_lexer::{Lexer, LexerConfiguration};
use crate::ast::DeclarationFlags;
use crate::validation::*;
use super::Parser;


type Error = Result<(),&'static str>;

fn validate(program: &str, validator: Box<AstValidator>) -> Error {
    let file = File::new("test",program);
    let mut lexer = Lexer::create(file,LexerConfiguration::default());
    let stream = lexer.lex();
    let mut parser = Parser::new(stream);
    let result = parser.parse(false);
    if parser.all_raised_errors.len() != 0 {
        Err("program failed to parse")
    } else {
        let validated = validator.validate(&result);
        if validated {
            Ok(())
        } else {
            println!("given:\n{program}\n\nexpected:\n{validator}\n\ngot:\n{result}");
            Err("program failed to validate")
        }
    }
}

fn v_i<T: ToString>(i: T) -> Box<AstValidator> {
    v_integer_literal(BigInt::from_str(i.to_string().as_str()).unwrap())
}

fn v_f(f: f64) -> Box<AstValidator> {
    v_float_literal(f)
}


fn v_empty_def() -> Box<AstValidator> {
    v_variable_definition(
        DeclarationFlags::empty(),
        "x".to_string(),
        None
    )
}


#[macro_export]
macro_rules! object_map {
     {$($key:expr => $value:expr),*$(,)?} => {
         {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push(v_field_literal($key.to_string(),$value));
            )*
            temp_vec
         }
     };
}

#[macro_export]
macro_rules! field_map {
     {$($key:expr => $value:expr),*$(,)?} => {
         {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push(v_field(DeclarationFlags::empty(),Some($key.to_string()),$value));
            )*
            temp_vec
         }
     };
}


#[macro_export]
macro_rules! args {
     [$($key:expr => $value:expr),*$(,)?] => {
         {
             let mut temp_vec = Vec::new();
             $(
                let s = $key.to_string();
                temp_vec.push(v_argument(if s == "_" {None} else { Some(s) },$value));
             )*
             temp_vec
         }
     };
}


#[test]
fn empty_input_results_in_empty_ast() -> Error  {
    validate("",v_program(vec![]))
}

#[test]
fn named_blocks() -> Error {
    validate(
        "let x = :(name) {yield(name) 5;};",
        v_single(
            v_variable_declaration(
                v_empty_def(),
                v_named_block(
                    "name".to_string(),
                    vec![
                        v_named_break(
                            "name".to_string(),
                            v_i(5)
                        )
                    ]
                )
            )
        )
    )
}

#[test]
fn self_keyword() -> Error {
    validate(
        "fn x(self) -> bool => self.x;",
        v_single(
            v_function(
                DeclarationFlags::empty(),
                "x".to_string(),
                None,
                vec![v_self_value()],
                Some(v_bool()),
                v_subscription(
                    v_self_value(),
                    v_name("x")
                )
            )
        )
    )
}

#[test]
fn constant_self_keyword() -> Error {
    validate(
        "fn x(~self) -> bool => self.x;",
        v_single(
            v_function(
                DeclarationFlags::empty(),
                "x".to_string(),
                None,
                vec![v_const_self_value()],
                Some(v_bool()),
                v_subscription(
                    v_self_value(),
                    v_name("x")
                )
            )
        )
    )
}

#[test]
fn self_type() -> Error {
    validate(
        "fn x(self) -> Self => self;",
        v_single(
            v_function(
                DeclarationFlags::empty(),
                "x".to_string(),
                None,
                vec![v_self_value()],
                Some(v_self_type()),
                v_self_value()
            )
        )
    )
}

/// This tests an example program against the parser to see if it parses correctly
#[test]
fn full_program() -> Error {
    validate(include_str!("tests/full_program.cheese"),v_program(
        vec![
            v_function(DeclarationFlags::empty(),"abs".to_string(),None, args!("arg" => v_float64()),Some(v_float64()),v_if(
                v_lesser_than(v_name("arg"),v_i(0)),
                None,
                v_unary_minus(v_name("arg")),
                Some(v_name("arg"))
            )),
            v_type_declaration(DeclarationFlags::empty(),"HSL".to_string(),None,v_structure(
                false,
                vec![],
                vec![
                    v_field(DeclarationFlags::empty(),Some("h".to_string()),v_float64()),
                    v_field(DeclarationFlags::empty(),Some("s".to_string()),v_float64()),
                    v_field(DeclarationFlags::empty(),Some("l".to_string()),v_float64()),
                    v_function(DeclarationFlags::public,"to_rgb".to_string(),None,vec![v_const_self_value()],Some(v_name("RGB")),v_block(
                        vec![
                            v_variable_declaration(
                                v_variable_definition(DeclarationFlags::empty(), "c".to_string(), None),
                                v_multiplication(
                                    v_subtraction(
                                        v_f(1.0),
                                        v_tuple_call(
                                            v_name("abs"),
                                            vec![
                                                v_subtraction(
                                                    v_multiplication(
                                                        v_f(2.0),
                                                        v_subscription(
                                                            v_self_value(),
                                                            v_name("l")
                                                        )
                                                    ),
                                                    v_f(1.0)
                                                )
                                            ]
                                        )
                                    ),
                                    v_subscription(
                                        v_self_value(),
                                        v_name("s")
                                    )
                                )
                            ),
                            v_variable_declaration(
                                v_variable_definition(DeclarationFlags::empty(), "x".to_string(), None),
                                v_multiplication(
                                    v_name("c"),
                                    v_subtraction(
                                        v_f(1.0),
                                        v_tuple_call(
                                            v_name("abs"),
                                            vec![
                                                v_subtraction(
                                                    v_modulus(
                                                        v_division(
                                                            v_subscription(
                                                                v_self_value(),
                                                                v_name("h")
                                                            ),
                                                            v_f(60.0)
                                                        ),
                                                        v_f(2.0)
                                                    ),
                                                    v_f(1.0)
                                                )
                                            ]
                                        )
                                    )
                                )
                            ),
                            v_variable_declaration(
                                v_variable_definition(DeclarationFlags::empty(), "m".to_string(), None),
                                v_subtraction(
                                    v_subscription(
                                        v_self_value(),
                                        v_name("l")
                                    ),
                                    v_division(
                                        v_name("c"),
                                        v_f(2.0)
                                    )
                                )
                            ),
                            v_variable_declaration(
                                v_variable_definition(DeclarationFlags::empty(), "h60".to_string(), None),
                                v_cast(
                                    v_division(
                                        v_subscription(
                                            v_self_value(),
                                            v_name("h")
                                        ),
                                        v_f(60.0)
                                    ),
                                    v_unsigned_integer_type(8)
                                )
                            ),
                            v_destructure(
                                v_tuple_destructure(
                                    vec![
                                        v_variable_definition(DeclarationFlags::empty(),"rprime".to_string(),None),
                                        v_variable_definition(DeclarationFlags::empty(),"gprime".to_string(),None),
                                        v_variable_definition(DeclarationFlags::empty(),"bprime".to_string(),None),
                                    ]
                                ),
                                v_match(
                                    v_name("h60"),
                                    vec![
                                        v_match_arm(
                                            vec![v_match_value(v_i(0))],
                                            None,
                                            v_tuple_literal(vec![v_name("c"),v_name("x"),v_f(0.0)])
                                        ),
                                        v_match_arm(
                                            vec![v_match_value(v_i(1))],
                                            None,
                                            v_tuple_literal(vec![v_name("x"),v_name("c"),v_f(0.0)])
                                        ),
                                        v_match_arm(
                                            vec![v_match_value(v_i(2))],
                                            None,
                                            v_tuple_literal(vec![v_f(0.0),v_name("x"),v_name("c")])
                                        ),
                                        v_match_arm(
                                            vec![v_match_value(v_i(3))],
                                            None,
                                            v_tuple_literal(vec![v_name("x"),v_f(0.0),v_name("c")])
                                        ),
                                        v_match_arm(
                                            vec![v_match_value(v_i(4))],
                                            None,
                                            v_tuple_literal(vec![v_name("c"),v_f(0.0),v_name("x")])
                                        ),
                                        v_match_arm(
                                            vec![v_match_all()],
                                            None,
                                            v_tuple_literal(vec![v_f(0.0),v_f(0.0),v_f(0.0)])
                                        ),

                                    ]
                                )
                            ),
                            v_implicit_result(v_object_literal(object_map![
                                "r" => v_cast(
                                    v_multiplication(
                                        v_addition(
                                            v_name("rprime"),
                                            v_name("m")
                                        ),
                                        v_i(255)
                                    ),
                                    v_unsigned_integer_type(8)
                                ),
                                "g" => v_cast(
                                    v_multiplication(
                                        v_addition(
                                            v_name("gprime"),
                                            v_name("m")
                                        ),
                                        v_i(255)
                                    ),
                                    v_unsigned_integer_type(8)
                                ),
                                "b" => v_cast(
                                    v_multiplication(
                                        v_addition(
                                            v_name("bprime"),
                                            v_name("m")
                                        ),
                                        v_i(255)
                                    ),
                                    v_unsigned_integer_type(8)
                                ),
                            ]))
                        ]
                    ))
                ]
            )),
            v_type_declaration(DeclarationFlags::empty(),"RGB".to_string(),None,v_structure(
                false,
                vec![],
                field_map!{
                    "r" => v_unsigned_integer_type(8),
                    "g" => v_unsigned_integer_type(8),
                    "b" => v_unsigned_integer_type(8),
                }
            )),
            v_function(DeclarationFlags::entry,"main".to_string(),None,vec![],Some(v_unsigned_integer_type(8)),v_block(vec![
                v_variable_declaration(
                    v_variable_definition(DeclarationFlags::empty(),"hsl".to_string(),None),
                    v_object_call(
                        v_name("HSL"),
                        object_map![
                            "h" => v_f(130.0),
                            "s" => v_f(0.5),
                            "l" => v_f(0.5)
                        ]
                    )
                ),
                v_tuple_call(v_name("puts"),vec![v_string("HSL(130,0.5,0.5) -> RGB = ")]),
                v_variable_declaration(
                    v_variable_definition(DeclarationFlags::empty(),"rgb".to_string(),None),
                    v_tuple_call(
                        v_subscription(
                            v_name("hsl"),
                            v_name("to_rgb")
                        ),
                        vec![]
                    )
                ),
                v_tuple_call(v_name("puts"),vec![v_string("R: ")]),
                v_tuple_call(
                    v_name("put_int"),
                    vec![
                        v_subscription(
                            v_name("rgb"),
                            v_name("r")
                        )
                    ]
                ),
                v_tuple_call(v_name("puts"),vec![v_string(", G: ")]),
                v_tuple_call(
                    v_name("put_int"),
                    vec![
                        v_subscription(
                            v_name("rgb"),
                            v_name("g")
                        )
                    ]
                ),
                v_tuple_call(v_name("puts"),vec![v_string(", B: ")]),
                v_tuple_call(
                    v_name("put_int"),
                    vec![
                        v_subscription(
                            v_name("rgb"),
                            v_name("b")
                        )
                    ]
                ),
                v_assign(
                    v_underscore(),
                    v_tuple_call(
                        v_name("putchar"),
                        vec![
                            v_i(10)
                        ]
                    )
                ),
                v_implicit_result(v_i(0))
            ])),
            v_function(DeclarationFlags::empty(),"put_int".to_string(),None,args!("i" => v_unsigned_integer_type(64)),Some(v_void()),v_block(vec![
                v_variable_definition(
                    DeclarationFlags::mutable,
                    "chars".to_string(),
                    Some(v_array_type(
                        false,
                        vec![v_i(21)],
                        v_unsigned_integer_type(8)
                    ))
                ),
                v_assign(
                    v_underscore(),
                    v_tuple_call(
                        v_name("puts"),
                        vec![
                            v_tuple_call(
                                v_name("_ui64toa"),
                                vec![
                                    v_name("i"),
                                    v_name("chars"),
                                    v_i(10)
                                ]
                            )
                        ]
                    )
                )
            ])),
            v_function(DeclarationFlags::empty(),"puts".to_string(),None,args!("s" => v_array_type(true,vec![v_unknown_size()],v_unsigned_integer_type(8))),Some(v_void()),v_block(vec![
                v_variable_declaration(
                    v_variable_definition(DeclarationFlags::mutable,"i".to_string(),Some(v_unsigned_integer_type(64))),
                    v_i(0)
                ),
                v_implicit_result(
                    v_while(
                        v_not_equal_to(
                            v_array_call(
                                v_name("s"),
                                vec![v_name("i")]
                            ),
                            v_i(0)
                        ),
                        v_block(vec![
                            v_assign(
                                v_underscore(),
                                v_tuple_call(
                                    v_name("putchar"),
                                    vec![v_array_call(
                                        v_name("s"),
                                        vec![v_name("i")]
                                    )],
                                )
                            ),
                            v_add_assign(
                                v_name("i"),
                                v_i(1)
                            )
                        ]),
                        None
                    )
                )
            ])),
            v_function_import(DeclarationFlags::public,"putchar".to_string(),args!("c" => v_unsigned_integer_type(8)),Some(v_signed_integer_type(32)),None),
            v_function_import(DeclarationFlags::public,"atoi".to_string(),args!("s" => v_array_type(true,vec![v_unknown_size()],v_unsigned_integer_type(8))),Some(v_signed_integer_type(32)),None),

            v_function_import(DeclarationFlags::public,"_ui64toa".to_string(),args!(
                "value" => v_unsigned_integer_type(64),
                "buffer" => v_array_type(false, vec![v_unknown_size()],v_unsigned_integer_type(8)),
                "radix" => v_signed_integer_type(32),
            ),Some(v_array_type(true,vec![v_unknown_size()],v_unsigned_integer_type(8))),None),
        ]
    ))
}
