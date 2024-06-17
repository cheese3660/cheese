use cheese_diagnostics::locating::File;
use cheese_lexer::{Lexer, LexerConfiguration};
use super::*;

#[test]
fn empty_input_results_in_empty_ast() {
    let file = File::new("test","");
    let mut lexer = Lexer::create(file,LexerConfiguration::default());
    let stream = lexer.lex();
    let mut parser = Parser::new(stream);
    let result = parser.parse();
    assert!(match result.data {
        Structure {
            is_tuple: false, ref interfaces, ref children
        } => interfaces.is_empty() && children.is_empty(),
        _ => false
    },"returned ast:\n{}",result)
}