use std::io::ErrorKind::UnexpectedEof;
use ariadne::Color;
use cheese_diagnostics::ErrorCode::{ExpectedCommaOrClose, ExpectedEndOfFile, ExpectedEquals, ExpectedImportPath, ExpectedModuleStatement, ExpectedName, ExpectedSemicolon, UnexpectedEndOfFile, UnexpectedFlags};
use cheese_diagnostics::locating::{Coordinate, FileSpan};
use cheese_diagnostics::ReportLabel;
use cheese_lexer::TokenType;
use cheese_lexer::TokenType::{Colon, Comma, EndOfFile, Identifier, RightBrace};
use crate::ast::{AstNode, AstNodeData, DeclarationFlags, NodeList, NodePtr};
use crate::ast::AstNodeData::{Import, ImportStar, ImportTree, Module};
use crate::parser::{ExpectedInformation, NO_NOTE, Parser};
use crate::try_err;

// This will specifically implement module parsing
impl Parser {
    // Do the rust way of being able to include other files as a module, as in module x; which finds a nearby file (or a subfolder if this is already a submodule)
    pub(super) fn parse_module(&mut self) -> NodePtr {
        let result = AstNode::new(self.peek_ref().span.clone(),Module(self.parse_module_statements()));
        if self.peek_ref().token_type != EndOfFile {
            _ = self.unexpected(self.peek(),"end of file", ExpectedEndOfFile,NO_NOTE,vec![]);
        }
        result
    }

    fn parse_module_statements(&mut self) -> NodeList {
        let mut list = vec![];
        while match self.peek_ref().token_type {
            EndOfFile | RightBrace => false,
            _ => true
        } {
            list.push(self.parse_module_statement())
        }
        list
    }

    // The flags are in case this is a function import

    fn parse_import_statement(&mut self, start_location: FileSpan, flags: DeclarationFlags) -> NodePtr {
        self.eat_any();
        match self.peek_ref().token_type {
            TokenType::LeftParentheses | TokenType::Fn => self.parse_imported_function(start_location,flags),
            _ => {
                if flags != DeclarationFlags::empty() {
                    self.raise(start_location.clone(),start_location.begin.clone(),UnexpectedFlags,"import statements do not have flags associated with them", NO_NOTE, vec![
                        ReportLabel::new(
                            start_location.clone(),
                            "unexpected flags found here",
                            None
                        )
                    ])
                } else {
                    let node = self.parse_import_tree_node();
                    let semi_loc = self.peek_ref().span.clone();
                    self.eat(TokenType::Semicolon,|tt| ExpectedInformation::new("a semicolon", ExpectedSemicolon, NO_NOTE, vec![])).unwrap_or_else(
                        || AstNode::new(start_location.expanded(&semi_loc),Import(node))
                    )
                }
            }
        }
    }

    fn parse_import_tree_node(&mut self) -> NodePtr {
        match self.peek_ref().token_type {
            TokenType::Star => {
                let result = AstNode::new(self.peek_ref().span.clone(), ImportStar);
                self.eat_any();
                result
            },
            Identifier => {
                let mut current_span = self.peek_ref().span.clone();
                let current_name = self.peek_ref().value.clone();
                let mut children = vec![];
                let mut last_span = current_span.clone();
                self.eat_any();
                match self.peek_ref().token_type {
                    TokenType::DoubleColon => {
                        self.eat_any();
                        match self.peek_ref().token_type {
                            TokenType::LeftBrace => {
                                self.eat_any();
                                let (mut children2, span, _) = self.parse_comma_separated_list(Self::parse_import_tree_node, |t| t == RightBrace, "}");
                                last_span = span;
                                children.append(&mut children2);
                            },
                            _ => {
                                let child = self.parse_import_tree_node();
                                last_span = child.span.clone();
                                children.push(child);
                            }
                        }
                    }
                    _ => {}
                }
                AstNode::new(current_span.expanded(&last_span),ImportTree { name: current_name, children })
            },
            _ => self.unexpected(self.peek(),"'*' or an identifier for a node in an import tree", ExpectedImportPath, NO_NOTE, vec![])
        }
    }

    fn parse_static(&mut self, start_location: FileSpan, flags: DeclarationFlags) -> NodePtr {
        self.eat_any();
        let name = self.peek_ref().value.clone();
        try_err!(self.eat(Identifier, |tt| ExpectedInformation::new("a name for a static variable", ExpectedName, NO_NOTE, vec![
            ReportLabel::new(
                start_location.clone(),
                "static variable declaration begins here",
                Some(Color::Blue)
            )
        ])));
        let variable_type = if self.peek_ref().token_type == Colon {
            self.eat_any();
            Some(self.parse_type())
        } else {
            None
        };
        try_err!(self.eat(TokenType::Assign, |tt|ExpectedInformation::new(format!("an '=' for the initial value of static variable {name}"), ExpectedEquals,NO_NOTE, vec![
            ReportLabel::new(
                start_location.clone(),
                "static variable declaration begins here",
                Some(Color::Blue)
            )
        ])));
        let value = self.parse_expression();
        let end_loc = self.peek_ref().span.clone();
        try_err!(self.eat(TokenType::Semicolon, |tt|ExpectedInformation::new(format!("a ';' following the static variable declaration for {name}"), ExpectedSemicolon, NO_NOTE, vec![
            ReportLabel::new(
                start_location.clone(),
                "static variable declaration begins here",
                Some(Color::Blue)
            )
        ])));
        AstNode::new(start_location.expanded(&end_loc),AstNodeData::StaticVariableDeclaration {
            flags,
            name,
            variable_type,
            value,
        })
    }
    fn parse_module_statement(&mut self) -> NodePtr {
        let start_token = self.peek().clone();
        let start_location = start_token.span.clone();
        let statement_flags = self.parse_flags();
        match self.peek_ref().token_type {
            TokenType::Import => self.parse_import_statement(start_location,statement_flags),
            TokenType::Static => self.parse_static(start_location,statement_flags),
            _ => {
                let result = self.unexpected(self.peek().clone(),"a top level statement", ExpectedModuleStatement, NO_NOTE, vec![]);
                self.eat_any();
                result
            }
        }
    }
}