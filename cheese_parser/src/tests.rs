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


fn v_empty_def() -> Box<AstValidator> {
    v_variable_definition(
        DeclarationFlags::empty(),
        "x".to_string(),
        None
    )
}


#[macro_export]
macro_rules! object_map {
     ($($key:expr => $value:expr),*$(,)?) => {
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
     ($($key:expr => $value:expr),*$(,)?) => {
         {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push(v_field(DeclarationFlags::empty(),Some($key.to_string()),$value));
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
fn full_program() -> Error {
    validate(include_str!("tests/full_program.cheese"),v_program(
        vec![]
    ))
}
