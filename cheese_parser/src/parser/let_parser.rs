use ariadne::{Color, Fmt};
use cheese_diagnostics::ErrorCode::{ExpectedEquals, ExpectedName};
use cheese_diagnostics::locating::FileSpan;
use cheese_diagnostics::ReportLabel;
use cheese_lexer::TokenType;
use cheese_lexer::TokenType::{Assign, Colon, Identifier, RightBracket, RightParentheses};
use crate::ast::{AstNode, DeclarationFlags, NodePtr, OptionalNode};
use crate::ast::AstNodeData::{ArrayDestructure, Destructure, StructureDestructure, StructureDestructureField, TupleDestructure, Underscore, VariableDeclaration, VariableDefinition};
use crate::parser::{ExpectedInformation, NO_NOTE, Parser};
use crate::try_err;

impl Parser {

    pub(super) fn parse_let(&mut self) -> NodePtr{
        let start_location = self.peek_ref().span.clone();
        self.eat_any();
        if match self.peek_ref().token_type {
            TokenType::LeftBrace | TokenType::LeftBracket | TokenType::LeftParentheses | TokenType::Underscore => true,
            _ => false
        } {
            return self.parse_destructure(start_location);
        }
        let mut def = self.parse_variable_definition();
        if self.peek_ref().token_type == Assign {
            self.eat_any();
            let value = self.parse_expression();
            AstNode::new(start_location.expanded(&value.span), VariableDeclaration {
                definition: def,
                value
            })
        } else {
            def.span = start_location.expanded(&def.span);
            def
        }
    }




    // returns (flag, name, type, last_token_location)
    fn parse_variable_definition(&mut self) -> NodePtr {
        let start_loc = self.peek_ref().span.clone();
        let flags = self.parse_flags();
        let name = self.peek_ref().value.clone();
        try_err!(self.eat(Identifier, |_|ExpectedInformation::new("Expected variable name", ExpectedName, NO_NOTE, vec![])));
        let (end_location, variable_type) = if self.peek_ref().token_type == Colon {
            self.eat_any();
            let t = self.parse_type();
            (t.span.clone(), Some(t))
        } else {
            (self.peek_ref().span.clone(), None)
        };
        AstNode::new(start_loc.expanded(&end_location),VariableDefinition {
            flags,
            name,
            variable_type,
        })
    }

    fn parse_destructure(&mut self, location: FileSpan) -> NodePtr {
        let structure = self.parse_destructure_statement();
        self.eat(Assign,|_|ExpectedInformation::new(
            format!("a '{}' following a destructure specifier",'='.fg(Color::Green)),
            ExpectedEquals,
            NO_NOTE,
            vec![
                ReportLabel::new(
                    location.clone(),
                    "destructure begins here",
                    Some(Color::Blue)
                ),
                ReportLabel::new(
                    structure.span.clone(),
                    format!("expected '{}' after this destructure specifier", '='.fg(Color::Green)),
                    Some(Color::Green)
                )
            ]
        )).unwrap_or_else(|| {
            let value = self.parse_expression();
            AstNode::new(
                location.expanded(&value.span),
                Destructure {
                    structure,
                    value
                }
            )
        })
    }
    fn parse_destructure_statement(&mut self) -> NodePtr {
        match self.peek_ref().token_type {
            TokenType::Underscore => {
                let location = self.peek_ref().span.clone();
                self.eat_any();
                AstNode::new(location, Underscore)
            },
            TokenType::LeftBracket => self.parse_destructure_array(),
            TokenType::LeftParentheses => self.parse_destructure_tuple(),
            TokenType::LeftBrace => self.parse_destructure_structure(),

            _ => self.parse_variable_definition()
        }
    }

    fn parse_destructure_array(&mut self) -> NodePtr {
        let start = self.peek_ref().span.clone();
        let (children, end, _) = self.parse_comma_separated_list(Self::parse_destructure_statement, |t| t == RightBracket, "]");
        AstNode::new(start.expanded(&end),ArrayDestructure(children))
    }

    fn parse_destructure_tuple(&mut self) -> NodePtr {
        let start = self.peek_ref().span.clone();
        let (children, end, _) = self.parse_comma_separated_list(Self::parse_destructure_statement, |t| t == RightParentheses, ")");
        AstNode::new(start.expanded(&end),TupleDestructure(children))
    }

    fn parse_destructure_structure(&mut self) -> NodePtr {
        let start = self.peek_ref().span.clone();
        let (children, end, _) = self.parse_comma_separated_list(Self::parse_structure_destructure_statement, |t| t == RightBracket, "}");
        AstNode::new(start.expanded(&end),StructureDestructure(children))
    }

    fn parse_structure_destructure_statement(&mut self) -> NodePtr {
        if self.peek_ref().token_type == Identifier && self.peek2() == Some(Colon) {
            let start_loc = self.peek_ref().span.clone();
            let name = self.peek_ref().value.clone();
            self.eat_any();
            self.eat_any();
            let def = self.parse_variable_definition();
            AstNode::new(start_loc.expanded(&def.span), StructureDestructureField {
                name,
                definition: def
            })
        } else {
            let def = self.parse_variable_definition();
            AstNode::new(def.span.clone(), StructureDestructureField {
                name: match &def.data {
                    VariableDefinition { name, ..} => name.clone(),
                    _ => unreachable!()
                },
                definition: def
            })
        }
    }

}