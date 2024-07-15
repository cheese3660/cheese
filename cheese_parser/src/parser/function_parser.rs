use ariadne::Color;
use cheese_diagnostics::ErrorCode::{ExpectedColon, ExpectedName};
use cheese_diagnostics::locating::FileSpan;
use cheese_diagnostics::ReportLabel;
use cheese_lexer::TokenType;
use cheese_lexer::TokenType::{Arrow, Colon, Comma, Identifier, LeftBracket, Pipe, RightBracket, Underscore};
use crate::ast::{AstNode, AstNodeData, DeclarationFlags, NodePtr};
use crate::ast::AstNodeData::{AnonymousFunction, Argument, Closure, ConstReferenceImplicitCapture, ConstSelfValue, InferredArgument, SelfValue};
use crate::parser::{ExpectedInformation, NO_NOTE, Parser};

impl Parser {
    pub(super) fn parse_imported_function(&mut self, start_location: FileSpan, flags: DeclarationFlags) -> NodePtr {
        todo!()
    }

    pub(super)
    fn parse_anonymous_function(&mut self) -> NodePtr {

    }

    fn parse_function_argument(&mut self) -> NodePtr {
        let start_loc = self.peek_ref().span.clone();
        match self.peek_ref().token_type {
            TokenType::SelfType => {
                self.eat_any();
                AstNode::new(start_loc, SelfValue)
            }
            TokenType::ConstSelfType => {
                self.eat_any();
                AstNode::new(start_loc, ConstSelfValue)
            }

            _ => {
                let name = if self.peek_ref().token_type == Underscore {
                    None
                } else if self.peek_ref().token_type == Identifier {
                    Some(self.peek_ref().value.clone())
                } else {
                    return self.unexpected(self.peek(), "argument name or '_' for a function argument", ExpectedName, NO_NOTE, vec![]);
                };
                self.eat_any();
                self.eat(Colon,|_|ExpectedInformation::new("expected colon to begin argument type",ExpectedColon, NO_NOTE, vec![
                    ReportLabel::new(
                        start_loc.clone(),
                        "expected after argument name here",
                        Some(Color::Cyan)
                    )
                ])).unwrap_or_else(
                    || {
                        let argument_type = self.parse_type();
                        AstNode::new(start_loc.expanded(&argument_type.span), Argument {
                            name,
                            argument_type
                        })
                    }
                )
            }
        }
    }


    // Used for closure arguments, they can omit types if it can be deduced by context
    fn parse_closure_argument(&mut self) -> NodePtr {
        let start_loc = self.peek_ref().span.clone();
        let name = if self.peek_ref().token_type == Underscore {
            None
        } else if self.peek_ref().token_type == Identifier {
            Some(self.peek_ref().value.clone())
        } else {
            return self.unexpected(self.peek(), "argument name or '_' for a closure argument", ExpectedName, NO_NOTE, vec![]);
        };
        self.eat_any();
        if self.peek_ref().token_type == Colon {
            self.eat_any();
            let argument_type = self.parse_type();
            AstNode::new(start_loc.expanded(&argument_type.span), Argument {
                name,
                argument_type
            })
        } else {
            match name {
                Some(name) => AstNode::new(start_loc, InferredArgument(name)),
                None => AstNode::new(start_loc, AstNodeData::Underscore)
            }
        }
    }

    pub(super)
    fn parse_closure(&mut self) -> NodePtr {
        let start_location = self.peek_ref().span.clone();
        self.eat_any();
        let (args, mut arg_end, _) = self.parse_comma_separated_list(Self::parse_closure_argument, |t| t == Pipe, "|");
        let captures = if self.peek_ref().token_type == LeftBracket {
            let (captures2, arg_end2, _) = self.parse_comma_separated_list(|t| t.parse_capture(true), |t| t == RightBracket, "]");
            arg_end = arg_end2;
            captures2
        } else {
            vec![AstNode::new(arg_end.clone(), ConstReferenceImplicitCapture)]
        };
        let return_type = if self.peek_ref().token_type == Arrow {
            self.eat_any();
            Some(self.parse_type())
        } else {
            None
        };
        let body = self.parse_expression();
        AstNode::new(start_location.expanded(&body.span), Closure {
            arguments,
            captures,
            return_type,
            body
        })
    }
}