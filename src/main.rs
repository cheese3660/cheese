use ariadne::{Color, ColorGenerator};
use cheese_lexer::*;
use cheese_diagnostics::locating::*;
use cheese_parser::Parser;
use cheese_utilities::trees::NodeBuilder;

fn main() {
    let file = File::new("test.cheese",include_str!("test.cheese"));
    let file2 = File::new("hsv.cheese",include_str!("hsv.cheese"));
    let mut lexer = Lexer::create(file2,LexerConfiguration::default());
    let stream = lexer.lex();
    // for tok in stream.clone() {
    //     println!("{tok}")
    // }
    let mut parser = Parser::new(stream);
    let result = parser.parse(false);
    println!("{result}");
    // Screw it lets build up a simple display
    // let mut cg = ColorGenerator::new();
    // println!("Simple inline node!");
    // let node = NodeBuilder::new("name",cg.next()).make_inline("main",cg.next()).build();
    // println!("{node}");
    // println!("Simple list node with unnamed values!");
    // let node = NodeBuilder::new("list",cg.next()).make_child("long silly name",cg.next()).make_child("longer silly name",cg.next()).build();
    // println!("{node}");
    // println!("Simple dictionary node!");
    // let node = NodeBuilder::new("dictionary",cg.next()).make_field("children","thirty-two",cg.next()).make_field("dead children","uncountably infinite",cg.next()).build();
    // println!("{node}");
}
