mod module_parser;
mod impl_parser;
mod type_parser;
mod function_parser;
mod trait_parser;
mod expression_parser;
mod let_parser;
mod match_parser;

use std::fmt::Display;
use ariadne::{Color, Fmt};
use cheese_diagnostics::{error, ErrorCode, exiting_error, ReportLabel};
use cheese_diagnostics::ErrorCode::{ExpectedCaptureName, ExpectedCaptureSpecifier, ExpectedClose, ExpectedCommaOrClose, ExpectedName, ExpectedOpenParentheses, GeneralCompilerError};
use cheese_diagnostics::locating::{Coordinate, File, FileSpan};
use cheese_lexer::{Token, TokenType};
use cheese_lexer::TokenType::{Assign, Colon, Comma, GreaterThan, Identifier, LeftParentheses};
use crate::ast::{AstNode, DeclarationFlags, NodeList, NodePtr, OptionalNode};
use crate::ast::AstNodeData::{ConstantReferenceCapture, ConstReferenceImplicitCapture, CopyCapture, CopyImplicitCapture, ErrorNode, False, ReferenceCapture, ReferenceImplicitCapture, Type};
const NO_NOTE: Option<&str> = None;

// This is just a module for parsers, that will each have their own file to separate stuff out
#[derive(Clone)]
pub struct RaisedError {
    error_code: ErrorCode,
    location: Coordinate,
    message: String,
}

struct ExpectedInformation {
    expected_message: String,
    error_code: ErrorCode,
    note: Option<String>,
    labels: Vec<ReportLabel>,
}

#[macro_export]
macro_rules! try_err {
    ($expr:expr) => {
        if let Some(_err) = $expr {
            return _err;
        }
    };

    ($expr:expr, $array:ident) => {
        if let Some(_err) = $expr {
            $array.push(_err);
        }
    }
}

impl ExpectedInformation {
    fn new<T: ToString, U: ToString>(expected_message: T, error_code: ErrorCode, note: Option<U>, labels: Vec<ReportLabel>) -> Self {
        ExpectedInformation {
            expected_message: expected_message.to_string(),
            error_code,
            note: match note {
                None => None,
                Some(n) => Some(n.to_string())
            },
            labels,
        }
    }
}


pub struct Parser {
    tokens: Vec<Token>,
    location: usize,
    pub all_raised_errors: Vec<RaisedError>,
}


// This is meant to be replica of the lib::parser methods, but split over multiple folders
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            location: 0,
            all_raised_errors: vec![],
        }
    }

    pub fn peek(&self) -> Token {
        self.tokens[self.location].clone()
    }

    pub fn peek_ref(&self) -> &Token {
        &self.tokens[self.location]
    }

    pub fn peek2(&self) -> Option<TokenType> {
        if self.location+1 < self.tokens.len() {
            Some(self.tokens[self.location+1].token_type.clone())
        } else {
            None
        }
    }

    pub fn previous(&mut self) {
        if self.location > 0 {
            self.location -= 1;
        }
    }

    pub fn eat<T: Fn(TokenType) -> ExpectedInformation/*,K: Fn(TokenType) -> bool*/>(&mut self, expected_type: TokenType, information: T/*, keep: K*/) -> OptionalNode {
        let token = self.peek_ref();
        if token.token_type != expected_type {
            let info = information(token.token_type);
            Some(self.unexpected(token.clone(), info.expected_message, info.error_code, info.note, info.labels))
        } else {
            self.eat_any();
            None
        }
    }


    pub fn eat_any(&mut self) {
        if self.location < self.tokens.len() - 1 {
            self.location += 1;
        }
    }

    pub fn unexpected<T: ToString, U: ToString>(&mut self, token: Token, expected: T, error_code: ErrorCode, note: Option<U>, mut labels: Vec<ReportLabel>) -> NodePtr {
        let clone = token;
        if clone.token_type == TokenType::EndOfFile {
            self.raise(clone.span.clone(), clone.span.begin, error_code, format!("unexpected {}. expected: {}", "EOF".fg(Color::Red), expected.to_string()), note, labels)
        } else {
            labels.push(ReportLabel::new(clone.span.clone(), format!("Unexpected '{}' found here", clone.value.clone().fg(Color::Red)), Some(Color::Red)));
            self.raise(clone.span.clone(), clone.span.begin, error_code, format!("unexpected token '{}'. expected: {}", clone.value.clone().fg(Color::Red), expected.to_string()), note, labels)
        }
    }

    pub fn raise<T: ToString, U: ToString>(&mut self, span: FileSpan, coordinate: Coordinate, error_code: ErrorCode, message: T, note: Option<U>, labels: Vec<ReportLabel>) -> NodePtr {
        self.all_raised_errors.push(RaisedError {
            error_code,
            message: message.to_string(),
            location: coordinate.clone(),
        });

        error(coordinate.clone(), error_code, message.to_string(), note, labels);

        return Box::new(AstNode {
            span,
            data: ErrorNode {
                error_code,
                message: message.to_string(),
            },
        });
    }

    pub fn parse(&mut self, exit_on_err: bool) -> NodePtr {
        let location = self.tokens[0].clone().span.begin;
        let program = self.parse_module();
        if exit_on_err {
            if self.all_raised_errors.len() == 1 {
                let first = self.all_raised_errors[0].clone();
                exiting_error(first.clone().location, first.error_code, first.message, NO_NOTE, vec![]);
            } else if self.all_raised_errors.len() > 1 {
                exiting_error(location, GeneralCompilerError, "multiple syntax errors detected".to_string(), NO_NOTE, vec![]);
            }
        }

        program
    }

    fn parse_flags(&mut self) -> DeclarationFlags {
        let mut result = DeclarationFlags::empty();
        loop {
            match self.peek_ref().token_type {
                TokenType::Inline => result |= DeclarationFlags::inline,
                TokenType::Extern => result |= DeclarationFlags::external,
                TokenType::Export => result |= DeclarationFlags::export,
                TokenType::CompileTime => result |= DeclarationFlags::comptime,
                TokenType::Public => result |= DeclarationFlags::public,
                TokenType::Private => result |= DeclarationFlags::private,
                TokenType::Mutable => result |= DeclarationFlags::mutable,
                TokenType::Entry => result |= DeclarationFlags::entry,
                TokenType::Implicit => result |= DeclarationFlags::implicit,
                TokenType::Explicit => result |= DeclarationFlags::explicit,
                _ => break result
            }
            self.eat_any()
        }
    }


    fn parse_comma_separated_list<T: Fn(&mut Parser) -> NodePtr, U: Fn(TokenType) -> bool, V: ToString>(&mut self, parse_function: T, end_method: U, end_str: V) -> (NodeList, FileSpan, TokenType) {
        let mut result = vec![];
        while match self.peek_ref().token_type {
            TokenType::EndOfFile => false,
            t => !end_method(t)
        } {
            let current = parse_function(self);
            if match self.peek_ref().token_type {
                Comma => false,
                t => !end_method(t)
            } {
                result.push(self.unexpected(self.peek(),format!("a ',' or '{}' following a value in a comma separated list",end_str.to_string()), ExpectedCommaOrClose, NO_NOTE, vec![
                    ReportLabel::new(
                        current.span.clone(),
                        "expected after this value",
                        None
                    )
                ]))
            }
            if self.peek_ref().token_type == Comma {
                self.eat_any()
            }
            result.push(current);
        }
        let end_loc = self.peek_ref().span.clone();
        let end_type = self.peek_ref().token_type;
        if !end_method(end_type) {
            result.push(self.unexpected(self.peek(), format!("expected '{}' to close comma separated list",end_str.to_string()), ExpectedClose, NO_NOTE, vec![]));
        }
        (result, end_loc, end_type)
    }

    // This method is used in multiple places hence why it is being put in the root parser impl
    fn parse_capture(&mut self, allow_implicit: bool) -> NodePtr {
        enum CaptureType {
            Copy,
            Reference,
            Const,
        }
        let mut capture_type = CaptureType::Copy;
        let front = self.peek_ref();
        let location = front.span.clone();
        match front.token_type {
            TokenType::Assign => self.eat_any(),
            TokenType::Star => {
                self.eat_any();
                capture_type = CaptureType::Reference;
            }
            TokenType::ConstantPointer => {
                self.eat_any();
                capture_type = CaptureType::Const;
            }
            Identifier | TokenType::Underscore => {}
            _ => return self.unexpected(front.clone(), format!("'{}', '{}', '{}', or an identifier for a capture", '*'.fg(Color::Green), "*~".fg(Color::Green), '='.fg(Color::Green)), ExpectedCaptureSpecifier, NO_NOTE, vec![])
        }
        let front = self.peek();
        if front.token_type == Identifier || front.token_type == TokenType::Underscore {
            let end = front.span.clone();
            let name = front.value;
            self.eat_any();
            AstNode::new(location.expanded(&end), match capture_type {
                CaptureType::Copy => CopyCapture(name),
                CaptureType::Reference => ReferenceCapture(name),
                CaptureType::Const => ConstantReferenceCapture(name)
            })
        } else if allow_implicit {
            AstNode::new(location, match capture_type {
                CaptureType::Copy => CopyImplicitCapture,
                CaptureType::Reference => ReferenceImplicitCapture,
                CaptureType::Const => ConstReferenceImplicitCapture
            })
        } else {
            self.unexpected(front, "a name for a capture in a context disallowing implicit captures", ExpectedCaptureName, Some("Cheese only allows implicit captures in closures."), vec![
                ReportLabel::new(
                    location,
                    "capture specifier begins here, expected name afterwards",
                    Some(Color::Green),
                )
            ])
        }
    }

    fn parse_generic_parameter_list(&mut self, allow_default: bool) -> (NodeList, FileSpan) {
        // Assume that we are already passed the first diamond
        let (a,b,_) = self.parse_comma_separated_list(|s| s.parse_generic_parameter(allow_default), |t| t == GreaterThan, ">");
        (a,b)
    }

    fn parse_generic_parameter(&mut self, allow_default: bool) -> NodePtr {
        let start_loc = self.peek_ref().span.clone();
        let name = self.peek_ref().value.clone();
        try_err!(self.eat(Identifier, |_| ExpectedInformation::new("a name for a generic parameter", ExpectedName, NO_NOTE, vec![])));
        let ty = if self.peek_ref().token_type == Colon {
            self.eat_any();
            Some(self.parse_type())
        } else {
            None
        };
        let default = if self.peek_ref().token_type == Assign {
            self.eat_any();
            Some(self.parse_generic_type_argument())
        } else {
            None
        };
    }
}