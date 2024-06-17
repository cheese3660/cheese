pub mod ast;
#[cfg(test)]
mod tests;
mod utilities;

use std::cmp::min;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::process::id;
use std::rc::Rc;
use bitflags::bitflags;
use cheese_diagnostics::{advice, error, ErrorCode, exiting_error, ReportLabel};
use cheese_diagnostics::locating::{Coordinate, FileSpan};
use cheese_lexer::{Token, TokenType};
use ast::AstNodeData::{ErrorNode, Field, Import, Structure};
use ariadne::{Color, Fmt, ColorGenerator};
use num_bigint::BigInt;
use serde_json::json;
use cheese_diagnostics::ErrorCode::{AbleToSimplifyGenerics, ExpectedArgumentClose, ExpectedArgumentDeclarationOrClose, ExpectedArgumentName, ExpectedBlockName, ExpectedCaptureClose, ExpectedCaptureName, ExpectedCaptureSpecifier, ExpectedClosingDiamond, ExpectedColon, ExpectedCommaOrClose, ExpectedEnumId, ExpectedFieldName, ExpectedFunctionInformation, ExpectedIdentifierOrOpeningDiamond, ExpectedImportName, ExpectedImportPath, ExpectedMatchingParentheses, ExpectedOpeningParentheses, ExpectedPrimary, ExpectedReturnType, ExpectedSemicolon, ExpectedSliceTypeClose, ExpectedStructureStatement, ExpectedType, GeneralCompilerError, IncorrectSeparator, InvalidCharacterLiteral, UnexpectedGenerics};
use cheese_lexer::TokenType::{Arrow, Block, Cast, Colon, Comma, ConstantPointer, Else, EndOfFile, GreaterThan, Identifier, LeftBracket, LeftParentheses, LessThan, Pipe, RightBrace, RightBracket, RightParentheses, Semicolon};
use cheese_utilities::strings::Escapable;
use crate::ast::{AstNode, AstNodeData, DeclarationFlags, NodeList, NodePtr, OptionalNode};
use crate::ast::AstNodeData::{AddressOf, AnonymousFunction, Argument, ArrayCall, ArrayLiteral, Break, BuiltinReference, Closure, Combine, CompileTimeComplex, CompileTimeFloat, CompileTimeInteger, CompileTimeString, Complex32, Complex64, ConstantReferenceCapture, ConstReferenceImplicitCapture, ConstSelfValue, Continue, CopyCapture, CopyImplicitCapture, Dereference, EmptyBreak, EmptyReturn, EnumLiteral, False, FieldLiteral, Float32, Float64, FloatLiteral, Function, FunctionPrototype, FunctionType, GenericInstanceReference, If, ImaginaryLiteral, IntegerLiteral, NamedBreak, NameReference, Not, ObjectCall, ObjectLiteral, Opaque, Reference, ReferenceCapture, ReferenceImplicitCapture, Return, SelfType, SelfValue, SignedIntegerType, SignedSize, Slice, Subscription, True, TupleCall, TupleLiteral, Type, TypeMemberReference, UnaryMinus, UnaryPlus, Underscore, UnsignedIntegerType, UnsignedSize, Void, Yield};
use crate::utilities::{bin_to_big_int, create_node_from_binop, dec_to_big_int, hex_to_big_int, is_binary_operand_type, is_binary_operation, is_statement_end, oct_to_big_int, precedence};


// Some stuff that needs to be done: generics
// fn x<...> () {
// }
//
// The template parameters are on the left hand side
// type X<...> is struct {
// }
//
// When we eventually add concepts.
// type X<...> is concept(T) {
// }
//
// Then generics are going to be done with ::<>, with type sub-parts being done with :: (like rust)

// Now let's start parsing! :3

#[derive(Clone)]
pub struct RaisedError {
    error_code: ErrorCode,
    location: Coordinate,
    message: String,
}


pub struct Parser {
    tokens: Vec<Token>,
    location: usize,
    all_raised_errors: Vec<RaisedError>,
}


fn builtin_remove_prefix(builtin: &str) -> String {
    builtin.trim_start_matches("$").to_string()
}

fn get_integer_type_size(value: &str) -> u64 {
    value.trim_start_matches("u").trim_start_matches("i").parse().unwrap()
}

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

    pub fn previous(&mut self) {
        if self.location > 0 {
            self.location -= 1;
        }
    }

    pub fn eat(&mut self, expected_type: TokenType, expected_message: String, error_code: ErrorCode, note: Option<String>, labels: Vec<ReportLabel>) -> OptionalNode {
        let token = self.peek_ref();
        if token.token_type != expected_type {
            Some(self.unexpected(token.clone(), expected_message, error_code, note, labels))
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

    pub fn unexpected(&mut self, token: Token, expected: String, error_code: ErrorCode, note: Option<String>, mut labels: Vec<ReportLabel>) -> NodePtr {
        let clone = token;
        if clone.token_type == TokenType::EndOfFile {
            self.raise(clone.span.clone(), clone.span.begin, error_code, format!("unexpected {}. expected: {}", "EOF".fg(Color::Red), expected), note, labels)
        } else {
            labels.push(ReportLabel::new(clone.span.clone(), format!("Unexpected '{}' found here", clone.value.clone().fg(Color::Red)), Some(Color::Red)));
            self.raise(clone.span.clone(), clone.span.begin, error_code, format!("unexpected token '{}'. expected: {}", clone.value.clone().fg(Color::Red), expected), note, labels)
        }
    }

    pub fn raise(&mut self, span: FileSpan, coordinate: Coordinate, error_code: ErrorCode, message: String, note: Option<String>, labels: Vec<ReportLabel>) -> NodePtr {
        self.all_raised_errors.push(RaisedError {
            error_code,
            message: message.clone(),
            location: coordinate.clone(),
        });

        error(coordinate.clone(), error_code, message.clone(), note, labels);

        return Box::new(AstNode {
            span,
            data: ErrorNode {
                error_code,
                message,
            },
        });
    }

    pub fn parse(&mut self) -> NodePtr {
        let location = self.tokens[0].clone().span.begin;
        let program = self.parse_program();
        if self.all_raised_errors.len() == 1 {
            let first = self.all_raised_errors[0].clone();
            exiting_error(first.clone().location, first.error_code, first.message, None, vec![]);
        } else if self.all_raised_errors.len() > 1 {
            exiting_error(location, GeneralCompilerError, "multiple syntax errors detected".to_string(), None, vec![]);
        }

        program
    }

    fn parse_program(&mut self) -> NodePtr {
        let mut children = vec![];
        let interfaces = vec![];
        let mut front = self.peek();
        let start = front.span.clone().begin;
        let mut last_was_field = false;
        let mut last_span = None;
        let mut last_end = front.span.clone().end;
        while front.token_type != EndOfFile {
            if front.token_type == Comma {
                if !last_was_field {
                    children.push(self.raise(
                        front.span.clone(),
                        front.span.clone().begin,
                        IncorrectSeparator,
                        "expected a semicolon to separate 2 non field declarations".to_string(),
                        None,
                        if let Some(s) = last_span.clone() {
                            vec![ReportLabel::new(s, "non field declaration found here".to_string(), Some(Color::Red)), ReportLabel::new(front.span.clone(), format!("consider replacing this '{}' with a '{}', or removing it.", ','.fg(Color::Yellow), ';'.fg(Color::Green)), Some(Color::Yellow))]
                        } else {
                            vec![ReportLabel::new(front.span.clone(), format!("consider replacing this '{}' with a '{}', or removing it.", ','.fg(Color::Yellow), ';'.fg(Color::Green)), Some(Color::Yellow))]
                        },
                    ))
                } else {}
                self.eat_any();
            } else if front.token_type == Semicolon {
                self.eat_any();
            } else {
                let last = self.parse_statement();
                last_span = Some(last.span.clone());
                if let Field { .. } = last.data {
                    last_was_field = true;
                } else {
                    last_was_field = false;
                }
                children.push(last);
            }

            front = self.peek();

            if front.token_type != EndOfFile {
                last_end = front.span.clone().end;
            }
        }

        Box::new(AstNode {
            span: FileSpan {
                begin: start,
                end: last_end,
            },
            data: Structure {
                is_tuple: false,
                interfaces,
                children,
            },
        })
    }

    fn parse_statement(&mut self) -> NodePtr {
        let front = self.peek_ref();
        let mut cg = ColorGenerator::new();
        match front.token_type {
            TokenType::Import => self.parse_import(),
            TokenType::Fn => self.parse_function(),
            _ => {
                let result = self.unexpected(front.clone(), format!(
                    "a structure statement (i.e. an {}, {}, {}, {}, {}, {}, {}, or a {})",
                    "import".fg(cg.next()),
                    "function declaration".fg(cg.next()),
                    "variable declaration".fg(cg.next()),
                    "variable definition".fg(cg.next()),
                    "type declaration".fg(cg.next()),
                    "comptime".fg(cg.next()),
                    "comptime conditional".fg(cg.next()),
                    "field".fg(cg.next())
                ), ExpectedStructureStatement, None, vec![]);
                self.eat_any(); // for error recovery
                result
            }
        }
    }

    fn parse_import(&mut self) -> NodePtr {
        let start_span = self.peek_ref().span.clone();
        let start = start_span.begin.clone();
        let mut end = start_span.end.clone();
        self.eat_any();
        let mut path = "".to_string();
        let mut resolved_name = "".to_string();
        let mut last_name = "".to_string();
        loop {
            let front = self.peek();
            match front.token_type {
                TokenType::Dot => path += "/",
                TokenType::Dereference => path += "..",
                TokenType::Identifier => {
                    path += front.value.as_str();
                    last_name = front.value.clone();
                }
                _ => break
            }
            self.eat_any();
            end = front.span.end.clone();
        }
        if path.is_empty() {
            return self.raise(FileSpan {
                begin: start.clone(),
                end: end.clone(),
            }, start.clone(), ExpectedImportPath, format!("missing import path following '{}' token", "import".fg(Color::Red)), None, vec![
                ReportLabel::new(FileSpan {
                    begin: start.clone(),
                    end: end.clone(),
                }, format!("expected path following this '{}' token", "import".fg(Color::Red)), Some(Color::Red))
            ]);
        }
        let front = self.peek_ref();
        let front_span = front.span.clone();
        if front.token_type == Cast {
            self.eat_any();
            let resolved = self.peek_ref();
            resolved_name = resolved.value.clone();
            end = resolved.span.end.clone();
            let ident = self.eat(
                Identifier,
                format!("an identifier following an '{}' for an {} statement", '@'.fg(Color::Green), "import".fg(Color::Blue)),
                ExpectedImportName,
                None,
                vec![
                    ReportLabel::new(
                        start_span,
                        format!("{} statement begins here", "import".fg(Color::Blue)),
                        Some(Color::Blue),
                    ),
                    ReportLabel::new(
                        front_span,
                        format!("expected identifier after '{}' here", '@'.fg(Color::Green)),
                        Some(Color::Green),
                    ),
                ],
            );
            if let Some(i) = ident {
                return i;
            }
        } else {
            resolved_name = last_name;
        }
        Box::new(AstNode {
            span: FileSpan {
                begin: start,
                end,
            },
            data: Import {
                path,
                name: resolved_name,
            },
        })
    }

    fn parse_argument(&mut self) -> NodePtr {
        let ident_token = self.peek();
        let span_begin = ident_token.span.clone();
        let mut name = None;
        match ident_token.token_type {
            Identifier => {
                name = Some(ident_token.value);
                self.eat_any();
            }
            TokenType::Underscore => {
                self.eat_any();
            }
            TokenType::SelfType => {
                self.eat_any();
                return AstNode::new(span_begin, SelfValue);
            }
            TokenType::ConstSelfType => {
                self.eat_any();
                return AstNode::new(span_begin, ConstSelfValue);
            }
            _ => return self.unexpected(
                ident_token,
                format!("'{}' or an identifier for an argument name", '_'.fg(Color::Cyan)),
                ExpectedArgumentName,
                None,
                vec![],
            )
        }
        let err = self.eat(
            Colon,
            format!("'{}' expected after argument name", ':'.fg(Color::Green)),
            ExpectedColon,
            None,
            vec![
                ReportLabel::new(
                    span_begin.clone(),
                    format!("'{}' expected after argument name here", ':'.fg(Color::Green)),
                    Some(Color::Cyan),
                ),
            ],
        );
        if let Some(result) = err {
            return result;
        }
        let argument_type = self.parse_type(true);
        let span_end = argument_type.span.clone();
        AstNode::new(FileSpan {
            begin: span_begin.begin,
            end: span_end.end,
        }, Argument {
            name,
            argument_type,
        })
    }

    fn parse_arguments(&mut self, end_token: TokenType, function_begin: FileSpan, function_type: &str, name: &str, argument_type: &str, end_str: &str, name_color: Color) -> NodeList {
        let mut result = vec![];
        let arg_list_begin = self.peek_ref().span.clone();
        self.eat_any();
        loop {
            let current = self.peek_ref();
            match current.token_type.clone() {
                Identifier | TokenType::Underscore | TokenType::SelfType | TokenType::ConstSelfType => {
                    let argument = self.parse_argument();
                    let peek_tok = self.peek_ref();
                    if peek_tok.token_type != Comma && peek_tok.token_type != end_token {
                        _ = self.unexpected(
                            peek_tok.clone(),
                            format!(
                                "a '{}' or '{}' following an argument in the {}argument list for the {} '{}'",
                                ','.fg(Color::Blue),
                                end_str.fg(Color::Blue),
                                argument_type,
                                function_type.fg(Color::Blue),
                                name.fg(Color::Blue)
                            ),
                            ExpectedCommaOrClose,
                            Some(format!("Cheese requires a comma between each argument in the {}argument list for {}s.", argument_type, function_type)),
                            vec![
                                ReportLabel::new(
                                    function_begin.clone(),
                                    format!("{} '{}' begins here", function_type.fg(Color::Blue), name.fg(name_color)),
                                    Some(name_color),
                                ),
                                ReportLabel::new(
                                    arg_list_begin.clone(),
                                    format!("{}argument list begins here", argument_type),
                                    Some(Color::Green),
                                ),
                                ReportLabel::new(
                                    argument.span.clone(),
                                    format!("argument is here, consider placing a '{}' afterwards", ','.fg(Color::Blue)),
                                    Some(Color::Blue),
                                ),
                            ],
                        );
                    } else if peek_tok.token_type == Comma {
                        self.eat_any();
                    }
                    result.push(argument);
                }
                c => {
                    if c == end_token {
                        self.eat_any();
                        break;
                    }
                    let error = self.unexpected(
                        current.clone(),
                        format!(
                            "the beginning of an argument, or a '{}' to end the {}argument list for the {} '{}'",
                            end_str.fg(Color::Green),
                            argument_type,
                            function_type.fg(Color::Blue),
                            name.fg(name_color)
                        ),
                        ExpectedArgumentDeclarationOrClose,
                        None,
                        vec![
                            ReportLabel::new(
                                function_begin.clone(),
                                format!("{} '{}' begins here", function_type.fg(Color::Blue), name.fg(name_color)),
                                Some(name_color),
                            ),
                            ReportLabel::new(
                                arg_list_begin.clone(),
                                format!("{}argument list begins here", argument_type),
                                Some(Color::Green),
                            ),
                        ],
                    );
                    let mut front_ty = c;
                    while match front_ty {
                        Identifier | TokenType::Underscore | EndOfFile => false,
                        c => c != end_token
                    } {
                        self.eat_any();
                        front_ty = self.peek_ref().token_type;
                    }
                    result.push(error);
                    if front_ty == EndOfFile {
                        break;
                    }
                }
            }
        }
        result
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
                _ => break result
            }
            self.eat_any()
        }
    }

    fn parse_function(&mut self) -> NodePtr {
        let front_ref = self.peek_ref();
        let front_span = front_ref.span.clone();
        self.eat_any();
        let name_tok = self.peek_ref();
        let ty = name_tok.token_type.clone();
        let mut name = name_tok.value.clone();
        let ident = self.eat(
            Identifier,
            format!("an identifier following an '{}' for a function declaration ", "fn".fg(Color::Blue)),
            ExpectedImportName,
            Some("Cheese does not support anonymous functions as top level statements".to_string()),
            vec![
                ReportLabel::new(
                    front_span.clone(),
                    format!("expected identifier after '{}' here", "fn".fg(Color::Blue)),
                    Some(Color::Blue),
                )
            ],
        );
        if let Some(result) = ident {
            name = "<error>".to_string();
            match ty {
                LeftParentheses | LessThan => {}
                _ => return result,
            }
        }
        let mut generic_arguments = None;
        let mut peek = self.peek_ref();
        if peek.token_type == LessThan {
            generic_arguments = Some(self.parse_arguments(GreaterThan, front_span.clone(), "function", name.as_ref(), "generic ", ">", Color::Yellow));
            peek = self.peek_ref();
        }
        if peek.token_type != LeftParentheses {
            return self.unexpected(
                peek.clone(),
                format!("'{}' to start argument list for function '{}'", '('.fg(Color::Green), name.as_str().fg(Color::Yellow)),
                ExpectedOpeningParentheses,
                None,
                vec![
                    ReportLabel::new(
                        front_span.clone(),
                        format!("function declaration for '{}' begins here", name.as_str().fg(Color::Yellow)),
                        Some(Color::Yellow),
                    )
                ],
            );
        }
        let arguments = self.parse_arguments(RightParentheses, front_span.clone(), "function", name.as_ref(), "", ")", Color::Yellow);
        // We should parse flags before the return type, as we might be returning a function type that could have flags
        let flags = self.parse_flags();
        let mut return_type = None;
        peek = self.peek_ref();
        if peek.token_type == Arrow {
            self.eat_any();
            return_type = Some(self.parse_type(true));
        }
        peek = self.peek_ref();
        let mut cg = ColorGenerator::new();
        match peek.token_type {
            TokenType::ThickArrow => {
                self.eat_any();
                let body = self.parse_expression();
                let end_loc = self.peek_ref().span.end.clone();
                let err = self.eat(Semicolon, format!("expected '{}' following expression bodied function '{}'", ';'.fg(Color::Green), name.as_str().fg(Color::Yellow)), ExpectedSemicolon, None, vec![
                    ReportLabel::new(
                        front_span.clone(),
                        format!("function declaration for '{}' begins here", name.as_str().fg(Color::Yellow)),
                        Some(Color::Yellow),
                    ),
                    ReportLabel::new(
                        body.span.clone(),
                        format!("expected '{}' after this expression", ';'.fg(Color::Green)),
                        Some(Color::Green),
                    ),
                ]);
                err.unwrap_or_else(|| AstNode::new(
                    FileSpan {
                        begin: front_span.begin,
                        end: end_loc,
                    },
                    Function {
                        flags,
                        name,
                        generic_arguments,
                        arguments,
                        return_type,
                        body,
                    }))
            }
            TokenType::LeftBrace => {
                let body = self.parse_expression_block();
                AstNode::new(FileSpan {
                    begin: front_span.begin,
                    end: body.span.end.clone(),
                }, Function {
                    flags,
                    name,
                    generic_arguments,
                    arguments,
                    return_type,
                    body,
                })
            }
            TokenType::Prototype => {
                let loc = self.peek_ref().span.clone();
                self.eat_any();
                match generic_arguments {
                    Some(_) => {
                        return self.raise(FileSpan {
                            begin: front_span.begin.clone(),
                            end: loc.end.clone(),
                        }, front_span.begin.clone(), UnexpectedGenerics, format!("Unexpected generic arguments for function prototype '{}'", name.as_str().fg(Color::Yellow)), Some("Function prototypes cannot have generic arguments in Cheese".to_string()), vec![
                            ReportLabel::new(
                                front_span.clone(),
                                format!("function declaration for '{}' begins here", name.as_str().fg(Color::Yellow)),
                                Some(Color::Yellow),
                            )]);
                    }
                    None => {}
                }

                let err = self.eat(Semicolon, format!("expected '{}' following function prototype '{}'", ';'.fg(Color::Green), name.as_str().fg(Color::Yellow)), ExpectedSemicolon, None, vec![
                    ReportLabel::new(
                        front_span.clone(),
                        format!("function declaration for '{}' begins here", name.as_str().fg(Color::Yellow)),
                        Some(Color::Yellow),
                    ),
                    ReportLabel::new(
                        loc.clone(),
                        format!("expected '{}' after the 'prototype' token here", ';'.fg(Color::Green)),
                        Some(Color::Green),
                    ),
                ]);
                err.unwrap_or_else(|| AstNode::new(
                    FileSpan {
                        begin: front_span.begin,
                        end: loc.end,
                    },
                    FunctionPrototype {
                        flags,
                        name,
                        arguments,
                        return_type,
                    }))
            }
            _ => self.unexpected(
                peek.clone(),
                format!("'{}', '{}', '{}', or '{}' following the function declaration for '{}'", "=>".fg(cg.next()), "{".fg(cg.next()), "import".fg(cg.next()), "prototype".fg(cg.next()), name.as_str().fg(Color::Yellow)),
                ExpectedFunctionInformation,
                None,
                vec![
                    ReportLabel::new(
                        front_span.clone(),
                        format!("function declaration for '{}' begins here", name.as_str().fg(Color::Yellow)),
                        Some(Color::Yellow),
                    )
                ],
            )
        }
    }

    fn parse_generic_argument_list(&mut self) -> NodeList {
        let mut result = vec![];
        self.eat_any();
        while match self.peek_ref().token_type {
            GreaterThan | EndOfFile => false,
            _ => true
        } {
            let arg = self.parse_generic_arg();
            if self.peek_ref().token_type != Comma && self.peek_ref().token_type != GreaterThan {
                _ = self.unexpected(self.peek(), format!("'{}' or '{}' after generic argument", ','.fg(Color::Green), '>'.fg(Color::Green)), ExpectedCommaOrClose, None, vec![
                    ReportLabel::new(arg.span.clone(), format!("perhaps add a '{}' or '{}' after this argument", ','.fg(Color::Green), '>'.fg(Color::Green)), Some(Color::Green))
                ]);
                while match self.peek_ref().token_type {
                    Comma | GreaterThan | EndOfFile => false,
                    _ => true
                } {
                    self.eat_any();
                }
            } else if self.peek_ref().token_type == Comma {
                self.eat_any();
            }
            result.push(arg);
        }
        result
    }

    fn parse_type(&mut self, unambiguous: bool) -> NodePtr {
        let mut base = self.parse_type_base();
        loop {
            let peek = self.peek_ref();
            let loc = peek.span.clone();
            match peek.token_type {
                TokenType::DoubleColon => {
                    self.eat_any();
                    let peek2 = self.peek_ref();
                    let loc2 = peek2.span.clone();
                    match peek2.token_type {
                        LessThan => {
                            if unambiguous {
                                advice(
                                    loc.begin.clone(),
                                    AbleToSimplifyGenerics,
                                    format!("unnecessary '{}', you can remove this as this in an unambiguous context", "::".fg(Color::Green)),
                                    Some(format!("Cheese only requires '{}' syntax for generics in places where it is ambiguous between a type and a comparison", "::<".fg(Color::Green))),
                                    vec![
                                        ReportLabel::new(
                                            loc.clone(),
                                            format!("you can remove this '{}'", "::".fg(Color::Green)),
                                            Some(Color::Green),
                                        )
                                    ],
                                );
                            }
                            self.eat_any();
                            let arguments = self.parse_generic_argument_list();
                            let end_pos = self.peek_ref().span.clone();
                            let err = self.eat(
                                GreaterThan,
                                format!("a '{}' to end a generic instance argument list", '>'.fg(Color::Green)),
                                ExpectedClosingDiamond,
                                None,
                                vec![
                                    ReportLabel::new(
                                        loc2,
                                        "generic instance argument list begins here".to_string(),
                                        Some(Color::Green),
                                    )
                                ],
                            );
                            if let Some(err) = err {
                                return err;
                            }
                            base = AstNode::new(FileSpan {
                                begin: base.span.begin.clone(),
                                end: end_pos.end,
                            }, GenericInstanceReference {
                                referee: base,
                                generic_args: arguments,
                            });
                        }
                        Identifier => {
                            let name = peek2.value.clone();
                            self.eat_any();
                            base = AstNode::new(FileSpan {
                                begin: base.span.begin.clone(),
                                end: loc2.end,
                            }, TypeMemberReference {
                                referee: base,
                                member: name,
                            })
                        }
                        _ => return self.unexpected(peek2.clone(), format!("an identifier or '{}'", '<'.fg(Color::Green)), ExpectedIdentifierOrOpeningDiamond, None, vec![
                            ReportLabel::new(
                                loc,
                                format!("expected identifier or '{}' after this '{}'", '<'.fg(Color::Green), "::".fg(Color::Green)),
                                Some(Color::Green),
                            )
                        ])
                    }
                }
                LessThan => {
                    if !unambiguous {
                        break;
                    }
                    self.eat_any();
                    let arguments = self.parse_generic_argument_list();
                    let end_pos = self.peek_ref().span.clone();
                    let err = self.eat(
                        GreaterThan,
                        format!("a '{}' to end a generic instance argument list", '>'.fg(Color::Green)),
                        ExpectedClosingDiamond,
                        None,
                        vec![
                            ReportLabel::new(
                                loc,
                                "generic instance argument list begins here".to_string(),
                                Some(Color::Green),
                            )
                        ],
                    );
                    if let Some(err) = err {
                        return err;
                    }
                    base = AstNode::new(FileSpan {
                        begin: base.span.begin.clone(),
                        end: end_pos.end,
                    }, GenericInstanceReference {
                        referee: base,
                        generic_args: arguments,
                    });
                }
                TokenType::Ampersand => {
                    self.eat_any();
                    let combined = self.parse_type(true);
                    base = AstNode::new(FileSpan {
                        begin: base.span.begin.clone(),
                        end: combined.span.end.clone(),
                    }, Combine {
                        lhs: base,
                        rhs: combined,
                    })
                }
                _ => break
            }
        }
        return base;
    }

    fn parse_type_base(&mut self) -> NodePtr {
        let start = self.peek();
        let start_location = self.location;
        let start_loc = start.span.clone();
        self.eat_any();
        match start.token_type {
            Identifier =>
                AstNode::new(start_loc, NameReference(start.value)),
            TokenType::BuiltinReference =>
                AstNode::new(start_loc, BuiltinReference(builtin_remove_prefix(start.value.as_str()))),
            TokenType::Float32 =>
                AstNode::new(start_loc, Float32),
            TokenType::Float64 =>
                AstNode::new(start_loc, Float64),
            TokenType::Complex32 =>
                AstNode::new(start_loc, Complex32),
            TokenType::Complex64 =>
                AstNode::new(start_loc, Complex64),
            TokenType::Type =>
                AstNode::new(start_loc, Type),
            TokenType::Void =>
                AstNode::new(start_loc, Void),
            TokenType::CompileTimeString =>
                AstNode::new(start_loc, CompileTimeString),
            TokenType::CompileTimeFloat =>
                AstNode::new(start_loc, CompileTimeFloat),
            TokenType::CompileTimeComplex =>
                AstNode::new(start_loc, CompileTimeComplex),
            TokenType::CompileTimeInt =>
                AstNode::new(start_loc, CompileTimeInteger),
            TokenType::UnsignedSize => AstNode::new(start_loc, UnsignedSize),
            TokenType::SignedSize => AstNode::new(start_loc, SignedSize),
            TokenType::Opaque => AstNode::new(start_loc, Opaque),
            TokenType::TypeSelf => AstNode::new(start_loc, SelfType),
            TokenType::UnsignedIntegerType => AstNode::new(start_loc, UnsignedIntegerType(get_integer_type_size(start.value.as_str()))),
            TokenType::SignedIntegerType => AstNode::new(start_loc, SignedIntegerType(get_integer_type_size(start.value.as_str()))),
            TokenType::Star | ConstantPointer => {
                let sub_type = self.parse_type(true);
                AstNode::new(FileSpan {
                    begin: start_loc.begin,
                    end: sub_type.span.clone().end,
                }, Reference {
                    constant: start.token_type == ConstantPointer,
                    subtype: sub_type,
                })
            }
            TokenType::Interface => self.parse_interface(start_loc),
            TokenType::Concept => self.parse_concept(start_loc),
            TokenType::Enum => self.parse_enum(start_loc),
            TokenType::Struct => self.parse_structure(start_loc),
            TokenType::LeftBracket => self.parse_array_type(start_loc),
            TokenType::Fn =>  {
                self.location = start_location;
                self.parse_function_type()
            },
            LessThan => self.parse_slice_type(start_loc),
            _ => self.unexpected(start.clone(), "a type".to_string(), ExpectedType, None, vec![])
        }
    }

    fn parse_function_type(&mut self) -> NodePtr {

        let (location, arguments, return_type, flags) = self.parse_function_begin(false);
        let return_type = return_type.unwrap();
        AstNode::new(location.expanded(&return_type.span),FunctionType {
            arguments,
            return_type,
            flags
        })
    }
    fn parse_interface(&mut self, start_location: FileSpan) -> NodePtr {
        todo!()
    }
    fn parse_concept(&mut self, start_location: FileSpan) -> NodePtr {
        todo!()
    }
    fn parse_enum(&mut self, start_location: FileSpan) -> NodePtr {
        todo!()
    }
    fn parse_structure(&mut self, start_location: FileSpan) -> NodePtr {
        todo!()
    }
    fn parse_array_type(&mut self, start_location: FileSpan) -> NodePtr {
        todo!()
    }
    fn parse_slice_type(&mut self, start_location: FileSpan) -> NodePtr {
        let peek = self.peek_ref();
        let constant = match peek.token_type {
            GreaterThan => false,
            TokenType::ConstantSlice => true,
            _ => return self.unexpected(
                peek.clone(),
                format!("'{}' or '{}' to denote a slice type", '>'.fg(Color::Green), ">~".fg(Color::Green)),
                ExpectedSliceTypeClose,
                None,
                vec![
                    ReportLabel::new(
                        start_location,
                        format!(
                            "expected '{}' or '{}' after this '{}' to close the slice type",
                            '>'.fg(Color::Green),
                            ">~".fg(Color::Green),
                            '<'.fg(Color::Yellow)),
                        Some(Color::Yellow),
                    )
                ],
            ),
        };
        self.eat_any();
        let subtype = self.parse_type(true);
        AstNode::new(
            start_location.expanded(&subtype.span),
            Slice {
                constant,
                subtype,
            },
        )
    }

    fn parse_generic_arg(&mut self) -> NodePtr {
        let mut base = self.parse_generic_arg_primary();
        loop {
            let peek = self.peek_ref();
            let loc = peek.span.clone();
            match peek.token_type {
                TokenType::DoubleColon => {
                    self.eat_any();
                    let peek2 = self.peek_ref();
                    let loc2 = peek2.span.clone();
                    match peek2.token_type {
                        LessThan => {
                            advice(
                                loc.begin.clone(),
                                AbleToSimplifyGenerics,
                                format!("unnecessary '{}', you can remove this as this in an unambiguous context", "::".fg(Color::Green)),
                                Some(format!("Cheese only requires '{}' syntax for generics in places where it is ambiguous between a type and a comparison", "::<".fg(Color::Green))),
                                vec![
                                    ReportLabel::new(
                                        loc.clone(),
                                        format!("you can remove this '{}'", "::".fg(Color::Green)),
                                        Some(Color::Green),
                                    )
                                ],
                            );
                            self.eat_any();
                            let arguments = self.parse_generic_argument_list();
                            let end_pos = self.peek_ref().span.clone();
                            let err = self.eat(
                                GreaterThan,
                                format!("a '{}' to end a generic instance argument list", '>'.fg(Color::Green)),
                                ExpectedClosingDiamond,
                                None,
                                vec![
                                    ReportLabel::new(
                                        loc2,
                                        "generic instance argument list begins here".to_string(),
                                        Some(Color::Green),
                                    )
                                ],
                            );
                            if let Some(err) = err {
                                return err;
                            }
                            base = AstNode::new(FileSpan {
                                begin: base.span.begin.clone(),
                                end: end_pos.end,
                            }, GenericInstanceReference {
                                referee: base,
                                generic_args: arguments,
                            });
                        }
                        Identifier => {
                            let name = peek2.value.clone();
                            self.eat_any();
                            base = AstNode::new(FileSpan {
                                begin: base.span.begin.clone(),
                                end: loc2.end,
                            }, TypeMemberReference {
                                referee: base,
                                member: name,
                            })
                        }
                        _ => return self.unexpected(peek2.clone(), format!("an identifier or '{}'", '<'.fg(Color::Green)), ExpectedIdentifierOrOpeningDiamond, None, vec![
                            ReportLabel::new(
                                loc,
                                format!("expected identifier or '{}' after this '{}'", '<'.fg(Color::Green), "::".fg(Color::Green)),
                                Some(Color::Green),
                            )
                        ])
                    }
                }
                LessThan => {
                    self.eat_any();
                    let arguments = self.parse_generic_argument_list();
                    let end_pos = self.peek_ref().span.clone();
                    let err = self.eat(
                        GreaterThan,
                        format!("a '{}' to end a generic instance argument list", '>'.fg(Color::Green)),
                        ExpectedClosingDiamond,
                        None,
                        vec![
                            ReportLabel::new(
                                loc,
                                "generic instance argument list begins here".to_string(),
                                Some(Color::Green),
                            )
                        ],
                    );
                    if let Some(err) = err {
                        return err;
                    }
                    base = AstNode::new(FileSpan {
                        begin: base.span.begin.clone(),
                        end: end_pos.end,
                    }, GenericInstanceReference {
                        referee: base,
                        generic_args: arguments,
                    });
                }
                TokenType::Ampersand => {
                    self.eat_any();
                    let combined = self.parse_type(true);
                    base = AstNode::new(FileSpan {
                        begin: base.span.begin.clone(),
                        end: combined.span.end.clone(),
                    }, Combine {
                        lhs: base,
                        rhs: combined,
                    })
                }
                _ => break
            }
        }
        return base;
    }

    // This is the primary used for generic parsing
    fn parse_generic_arg_primary(&mut self) -> NodePtr {
        let old_loc = self.location;
        let start = self.peek();
        let start_loc = start.span.clone();
        self.eat_any();
        match start.token_type {
            Identifier =>
                AstNode::new(start_loc, NameReference(start.value)),
            TokenType::BuiltinReference =>
                AstNode::new(start_loc, BuiltinReference(builtin_remove_prefix(start.value.as_str()))),
            TokenType::Float32 =>
                AstNode::new(start_loc, Float32),
            TokenType::Float64 =>
                AstNode::new(start_loc, Float64),
            TokenType::Complex32 =>
                AstNode::new(start_loc, Complex32),
            TokenType::Complex64 =>
                AstNode::new(start_loc, Complex64),
            TokenType::Type =>
                AstNode::new(start_loc, Type),
            TokenType::Void =>
                AstNode::new(start_loc, Void),
            TokenType::CompileTimeString =>
                AstNode::new(start_loc, CompileTimeString),
            TokenType::CompileTimeFloat =>
                AstNode::new(start_loc, CompileTimeFloat),
            TokenType::CompileTimeComplex =>
                AstNode::new(start_loc, CompileTimeComplex),
            TokenType::CompileTimeInt =>
                AstNode::new(start_loc, CompileTimeInteger),
            TokenType::UnsignedSize => AstNode::new(start_loc, UnsignedSize),
            TokenType::SignedSize => AstNode::new(start_loc, SignedSize),
            TokenType::Opaque => AstNode::new(start_loc, Opaque),
            TokenType::TypeSelf => AstNode::new(start_loc, SelfType),
            TokenType::UnsignedIntegerType => AstNode::new(start_loc, UnsignedIntegerType(get_integer_type_size(start.value.as_str()))),
            TokenType::SignedIntegerType => AstNode::new(start_loc, SignedIntegerType(get_integer_type_size(start.value.as_str()))),
            TokenType::Star | ConstantPointer => {
                let sub_type = self.parse_type(true);
                AstNode::new(FileSpan {
                    begin: start_loc.begin,
                    end: sub_type.span.clone().end,
                }, Reference {
                    constant: start.token_type == ConstantPointer,
                    subtype: sub_type,
                })
            }
            TokenType::Interface => self.parse_interface(start_loc),
            TokenType::Concept => self.parse_concept(start_loc),
            TokenType::Enum => self.parse_enum(start_loc),
            TokenType::Struct => self.parse_structure(start_loc),
            TokenType::LeftBracket => self.parse_array_type(start_loc),
            TokenType::Fn => {
                self.location = old_loc;
                self.parse_function_type()
            },
            LessThan => self.parse_slice_type(start_loc),
            _ => {
                // backtrack by one token
                self.location = old_loc;
                self.parse_primary(None)
            }
        }
    }

    fn parse_expression(&mut self) -> NodePtr {
        let primary = self.parse_primary(None);
        self.parse_expression_precedence(primary, 0)
    }

    fn parse_expression_precedence(&mut self, mut lhs: NodePtr, min_precedence: u8) -> NodePtr {
        let mut lookahead = self.peek_ref().token_type;
        while is_binary_operation(lookahead) && precedence(lookahead) > min_precedence {
            let op = lookahead;
            let op_precedence = precedence(lookahead);
            self.eat_any();
            let mut rhs = if is_binary_operand_type(op) {
                self.parse_type(false)
            } else {
                self.parse_primary(None)
            };
            lookahead = self.peek_ref().token_type;
            while is_binary_operation(lookahead) && precedence(lookahead) > op_precedence && !is_binary_operand_type(lookahead) {
                rhs = self.parse_expression_precedence(rhs, min_precedence + 1);
                lookahead = self.peek_ref().token_type;
            }
            lhs = create_node_from_binop(op, lhs, rhs);
        }
        lhs
    }

    fn parse_call_list(&mut self, ending_type: TokenType, ending_str: &str) -> (NodeList, FileSpan) {
        let opening_location = self.peek_ref().span.clone();
        self.eat_any();
        let mut result = vec![];
        while match self.peek_ref().token_type {
            EndOfFile => false,
            t => t != ending_type
        } {
            let argument = self.parse_expression();
            let peek_tok = self.peek_ref();
            if peek_tok.token_type != Comma && peek_tok.token_type != ending_type {
                _ = self.unexpected(peek_tok.clone(), format!("'{}' or '{}' following an argument in an argument list", ','.fg(Color::Green), ending_str.fg(Color::Green)), ExpectedCommaOrClose, None, vec![
                    ReportLabel::new(
                        opening_location.clone(),
                        "argument list begins here".to_string(),
                        Some(Color::Cyan),
                    ),
                    ReportLabel::new(
                        argument.span.clone(),
                        format!("expected '{}' or '{}' following this argument, consider inserting either", ','.fg(Color::Green), ending_str.fg(Color::Green)),
                        Some(Color::Yellow),
                    ),
                ]);
            } else if peek_tok.token_type == Comma {
                self.eat_any();
            }
            result.push(argument)
        }
        let end_location = self.peek_ref().span.clone();
        let err = self.eat(ending_type, format!("'{}' to end the argument list for a call", ending_str.fg(Color::Green)), ExpectedArgumentClose, None, vec![
            ReportLabel::new(
                opening_location.clone(),
                "argument list begins here".to_string(),
                Some(Color::Cyan),
            )]);
        if let Some(e) = err {
            result.push(e)
        }
        (result, end_location)
    }

    fn parse_object_list(&mut self) -> (NodeList, FileSpan) {
        let opening_location = self.peek_ref().span.clone();
        self.eat_any();
        let mut result = vec![];
        while match self.peek_ref().token_type {
            TokenType::RightBrace | EndOfFile => false,
            _ => true
        } {
            let name = self.peek_ref().value.clone();
            let name_loc = self.peek_ref().span.clone();
            let err = self.eat(Identifier, "a field name for a field in the field list for an object literal".to_string(), ExpectedFieldName, None, vec![
                ReportLabel::new(
                    opening_location.clone(),
                    "field list begins here".to_string(),
                    Some(Color::Cyan),
                )]);
            if let Some(e) = err {
                result.push(e);
            };
            let front_ty = self.peek_ref().token_type;
            let err = self.eat(Colon, format!("a '{}' following a field name for a field in the field list for an object literal", ':'.fg(Color::Green)), ExpectedColon, if front_ty == Block {
                Some(format!("'{}' without a space in between the '{}' and '{}' is lexed as a named block, consider adding a space", ":(".fg(Color::Green), ':'.fg(Color::Green), '('.fg(Color::Green)))
            } else {
                None
            }, vec![
                ReportLabel::new(
                    opening_location.clone(),
                    "field list begins here".to_string(),
                    Some(Color::Cyan),
                ),
                ReportLabel::new(
                    name_loc.clone(),
                    format!("field begins here, expected '{}' after this", ':'.fg(Color::Green)),
                    Some(Color::Yellow),
                ),
            ]);
            if let Some(e) = err {
                result.push(e);
            };
            let value = self.parse_expression();
            let argument = AstNode::new(
                name_loc.expanded(&value.span),
                FieldLiteral {
                    name,
                    value,
                },
            );
            let peek_tok = self.peek_ref();
            if peek_tok.token_type != Comma && peek_tok.token_type != RightBrace {
                _ = self.unexpected(peek_tok.clone(), format!("'{}' or '{}' following a field in the field list for an object literal", ','.fg(Color::Green), '}'.fg(Color::Green)), ExpectedCommaOrClose, None, vec![
                    ReportLabel::new(
                        opening_location.clone(),
                        "argument list begins here".to_string(),
                        Some(Color::Cyan),
                    ),
                    ReportLabel::new(
                        argument.span.clone(),
                        format!("expected '{}' or '{}' following this field, consider inserting either", ','.fg(Color::Green), '}'.fg(Color::Green)),
                        Some(Color::Yellow),
                    ),
                ]);
            } else if peek_tok.token_type == Comma {
                self.eat_any();
            }
            result.push(argument);
        }

        let end_location = self.peek_ref().span.clone();
        let err = self.eat(RightBracket, format!("'{}' to end the field list for an object literal", '}'.fg(Color::Green)), ExpectedArgumentClose, None, vec![
            ReportLabel::new(
                opening_location,
                "field list begins here".to_string(),
                Some(Color::Cyan),
            )]);
        if let Some(e) = err {
            result.push(e)
        }
        (result, end_location)
    }

    fn parse_primary(&mut self, lookbehind: OptionalNode) -> NodePtr {
        let mut base = lookbehind.unwrap_or_else(|| self.parse_primary_base());
        loop {
            let front = self.peek_ref();
            let loc = front.span.clone();
            match front.token_type {
                TokenType::Dot => {
                    self.eat_any();
                    let next = self.parse_primary_base();
                    base = AstNode::new(base.span.expanded(&next.span), Subscription {
                        lhs: base,
                        rhs: next,
                    })
                }
                LeftParentheses => {
                    let (args, end_span) = self.parse_call_list(RightParentheses, ")");
                    base = AstNode::new(base.span.expanded(&end_span), TupleCall {
                        functional: base,
                        args,
                    })
                }
                TokenType::LeftBracket => {
                    let (args, end_span) = self.parse_call_list(RightBracket, "]");
                    base = AstNode::new(base.span.expanded(&end_span), ArrayCall {
                        functional: base,
                        args,
                    })
                }
                TokenType::LeftBrace => {
                    let (args, end_span) = self.parse_object_list();
                    base = AstNode::new(base.span.expanded(&end_span), ObjectCall {
                        functional: base,
                        args,
                    })
                }
                TokenType::DoubleColon => {
                    self.eat_any();
                    let peek2 = self.peek_ref();
                    let loc2 = peek2.span.clone();
                    match peek2.token_type {
                        LessThan => {
                            self.eat_any();
                            let arguments = self.parse_generic_argument_list();
                            let end_pos = self.peek_ref().span.clone();
                            let err = self.eat(
                                GreaterThan,
                                format!("a '{}' to end a generic instance argument list", '>'.fg(Color::Green)),
                                ExpectedClosingDiamond,
                                None,
                                vec![
                                    ReportLabel::new(
                                        loc2,
                                        "generic instance argument list begins here".to_string(),
                                        Some(Color::Green),
                                    )
                                ],
                            );
                            if let Some(err) = err {
                                return err;
                            }
                            base = AstNode::new(FileSpan {
                                begin: base.span.begin.clone(),
                                end: end_pos.end,
                            }, GenericInstanceReference {
                                referee: base,
                                generic_args: arguments,
                            });
                        }
                        Identifier => {
                            let name = peek2.value.clone();
                            self.eat_any();
                            base = AstNode::new(FileSpan {
                                begin: base.span.begin.clone(),
                                end: loc2.end,
                            }, TypeMemberReference {
                                referee: base,
                                member: name,
                            })
                        }
                        _ => return self.unexpected(peek2.clone(), format!("an identifier or '{}'", '<'.fg(Color::Green)), ExpectedIdentifierOrOpeningDiamond, None, vec![
                            ReportLabel::new(
                                loc,
                                format!("expected identifier or '{}' after this '{}'", '<'.fg(Color::Green), "::".fg(Color::Green)),
                                Some(Color::Green),
                            )
                        ])
                    }
                }
                TokenType::Dereference => {
                    base = AstNode::new(base.span.expanded(&front.span), Dereference(base));
                    self.eat_any();
                }
                _ => break
            }
        }
        base
    }

    fn parse_primary_base(&mut self) -> NodePtr {
        let start = self.peek();
        let return_to = self.location;
        let start_loc = start.span.clone();
        self.eat_any();
        let mut go_back = || self.location = return_to;
        let mut cg = ColorGenerator::new();
        match start.token_type {
            TokenType::DecimalLiteral => AstNode::new(start_loc, IntegerLiteral(dec_to_big_int(start.value.as_str()))),
            TokenType::HexadecimalLiteral => AstNode::new(start_loc, IntegerLiteral(hex_to_big_int(start.value.as_str()))),
            TokenType::OctalLiteral => AstNode::new(start_loc, IntegerLiteral(oct_to_big_int(start.value.as_str()))),
            TokenType::BinaryLiteral => AstNode::new(start_loc, IntegerLiteral(bin_to_big_int(start.value.as_str()))),
            TokenType::FloatingLiteral => AstNode::new(start_loc, FloatLiteral(start.value.replace("_", "").parse().unwrap())),
            TokenType::ImaginaryLiteral => AstNode::new(start_loc, ImaginaryLiteral(start.value.replace("_", "").replace("I", "").parse().unwrap())),
            Identifier => AstNode::new(start_loc, NameReference(start.value)),
            TokenType::BuiltinReference => AstNode::new(start_loc, BuiltinReference(start.value.replace("$", ""))),
            TokenType::StringLiteral => AstNode::new(start_loc, BuiltinReference(start.value.unescape_with_quotes())),
            TokenType::CharacterLiteral => {
                let unescaped = start.value.unescape_with_quotes();
                let len = unescaped.chars().count();
                if len != 1 {
                    return self.raise(start_loc.clone(), start_loc.begin.clone(), InvalidCharacterLiteral, format!("invalid length for character literal: {len}, character literals must have 1 character"), None, vec![
                        ReportLabel::new(start_loc.clone(), "invalid character literal found here".to_string(), Some(Color::Red))
                    ]);
                }
                let first = unescaped.chars().next().unwrap();
                AstNode::new(start_loc, IntegerLiteral(BigInt::from(first as u32)))
            }
            TokenType::Ampersand => {
                let rest = self.parse_expression();
                AstNode::new(start_loc.expanded(&rest.span), AddressOf(rest))
            }
            TokenType::Dash => {
                let rest = self.parse_expression();
                AstNode::new(start_loc.expanded(&rest.span), UnaryMinus(rest))
            }
            TokenType::Plus => {
                let rest = self.parse_expression();
                AstNode::new(start_loc.expanded(&rest.span), UnaryPlus(rest))
            }
            TokenType::Not => {
                let rest = self.parse_expression();
                AstNode::new(start_loc.expanded(&rest.span), Not(rest))
            }
            TokenType::True => AstNode::new(start_loc, True),
            TokenType::False => AstNode::new(start_loc, False),
            TokenType::Pipe => {
                go_back();
                self.parse_closure()
            }
            TokenType::Fn => {
                go_back();
                self.parse_anonymous_function()
            }
            TokenType::None => AstNode::new(start_loc, AstNodeData::None),
            TokenType::Tuple => {
                go_back();
                let (args, end) = self.parse_call_list(RightParentheses, ")");
                AstNode::new(start_loc.expanded(&end), TupleLiteral(args))
            }
            TokenType::Array => {
                go_back();
                let (args, end) = self.parse_call_list(RightBracket, "]");
                AstNode::new(start_loc.expanded(&end), ArrayLiteral(args))
            }
            TokenType::Object => {
                go_back();
                let (args, end) = self.parse_object_list();
                AstNode::new(start_loc.expanded(&end), ObjectLiteral(args))
            }
            TokenType::Dot => {
                let id = self.peek_ref().value.clone();
                let loc = self.peek_ref().span.clone();
                let err = self.eat(Identifier, "an identifier for an enum literal".to_string(), ExpectedEnumId, None, vec![
                    ReportLabel::new(
                        start_loc.clone(),
                        format!("expected identifier after this '{}'", '.'.fg(Color::Green)),
                        Some(Color::Green),
                    )
                ]);
                err.unwrap_or_else(|| AstNode::new(start_loc.expanded(&loc), EnumLiteral(id)))
            }
            TokenType::LeftBrace => {
                go_back();
                self.parse_expression_block()
            }
            LeftParentheses => {
                let result = self.parse_expression();
                self.eat(RightParentheses, format!("'{}' to close off '{}'", ')'.fg(Color::Green), '('.fg(Color::Green)), ExpectedMatchingParentheses, None, vec![
                    ReportLabel::new(
                        start_loc.clone(),
                        format!("parenthetical begins at this '{}'", '('.fg(Color::Green)),
                        Some(Color::Green),
                    )
                ]).unwrap_or_else(|| result)
            }
            TokenType::Return => if !is_statement_end(self.peek_ref().token_type) {
                let value = self.parse_expression();
                AstNode::new(start_loc.expanded(&value.span), Return(value))
            } else {
                AstNode::new(start_loc, EmptyReturn)
            },
            TokenType::Break => if !is_statement_end(self.peek_ref().token_type) {
                let value = self.parse_expression();
                AstNode::new(start_loc.expanded(&value.span), Break(value))
            } else {
                AstNode::new(start_loc, EmptyBreak)
            },
            TokenType::Yield => {
                let value = self.parse_expression();
                AstNode::new(start_loc.expanded(&value.span), Yield(value))
            }
            TokenType::Continue => AstNode::new(start_loc, Continue),
            TokenType::BlockYield => {
                let front = self.peek();
                let block_name = front.value;
                self.eat(Identifier, "a name for a named yield".to_string(), ExpectedBlockName, Some(format!("You might be trying to yield a value, rather than from a named block, consider putting a space between the '{}' and '{}' tokens", "yield".fg(Color::Blue), '('.fg(Color::Green))), vec![
                    ReportLabel::new(
                        start_loc.clone(),
                        "named yield begins here, expected name afterwards".to_string(),
                        Some(Color::Blue),
                    )
                ]).unwrap_or_else(|| self.eat(RightParentheses, format!("A '{}' to close off a named block yield's name.", ')'.fg(Color::Green)), ExpectedMatchingParentheses, None, vec![ReportLabel::new(
                    start_loc.clone(),
                    "named yield begins here, expected name afterwards".to_string(),
                    Some(Color::Blue),
                )]).unwrap_or_else(|| {
                    let result = self.parse_expression();
                    AstNode::new(start_loc.expanded(&result.span), NamedBreak {
                        name: block_name,
                        value: result,
                    })
                }))
            }
            TokenType::CompileTime => {
                go_back();
                self.parse_comptime()
            }
            Block => {
                go_back();
                self.parse_named_block()
            }
            TokenType::If => {
                go_back();
                self.parse_if()
            }
            TokenType::Match => {
                go_back();
                self.parse_match()
            }
            TokenType::For => {
                go_back();
                self.parse_for()
            }
            TokenType::While => {
                go_back();
                self.parse_while()
            }
            TokenType::Loop => {
                go_back();
                self.parse_loop()
            }
            TokenType::Underscore => AstNode::new(start_loc, Underscore),
            _ => self.unexpected(start, format!(
                "any of the following: {}, {}, {}, {}, {}, {}, {}, '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', or '{}'.",
                "an integer literal".fg(cg.next()),
                "a floating point literal".fg(cg.next()),
                "an imaginary literal".fg(cg.next()),
                "an identifier".fg(cg.next()),
                "a builtin".fg(cg.next()),
                "a string literal".fg(cg.next()),
                "a character literal".fg(cg.next()),
                '&'.fg(cg.next()),
                '-'.fg(cg.next()),
                '+'.fg(cg.next()),
                "not".fg(cg.next()),
                "true".fg(cg.next()),
                "false".fg(cg.next()),
                '|'.fg(cg.next()),
                "fn".fg(cg.next()),
                "none".fg(cg.next()),
                ".(".fg(cg.next()),
                ".[".fg(cg.next()),
                ".{".fg(cg.next()),
                '.'.fg(cg.next()),
                '{'.fg(cg.next()),
                '('.fg(cg.next()),
                "return".fg(cg.next()),
                "continue".fg(cg.next()),
                "break".fg(cg.next()),
                "yield".fg(cg.next()),
                "comptime".fg(cg.next()),
                ":(".fg(cg.next()),
                "if".fg(cg.next()),
                "match".fg(cg.next()),
                "loop".fg(cg.next()),
                "self".fg(cg.next()),
                "for".fg(cg.next()),
                "while".fg(cg.next()),
                '_'.fg(cg.next())
            ), ExpectedPrimary, None, vec![])
        }
    }

    fn parse_closure(&mut self) -> NodePtr {
        let location = self.peek_ref().span.clone();
        let arguments = self.parse_arguments(Pipe, location.clone(), "closure", "<unnamed>", "", "|", Color::Yellow);
        let front = self.peek_ref();
        let mut captures = vec![];
        if front.token_type == LeftBracket {
            let capture_list_begin = location.clone();
            self.eat_any();
            while match self.peek_ref().token_type {
                EndOfFile | RightBracket => false,
                _ => true
            } {
                let capture = self.parse_capture(true);
                let front = self.peek_ref();
                if front.token_type != RightBracket && front.token_type != Comma {
                    _ = self.unexpected(
                        front.clone(),
                        format!(
                            "a '{}' or '{}' following a capture in the capture list for a closure",
                            ','.fg(Color::Blue),
                            ']'.fg(Color::Blue),
                        ),
                        ExpectedCommaOrClose,
                        None,
                        vec![
                            ReportLabel::new(
                                location.clone(),
                                "closure begins here".to_string(),
                                Some(Color::Yellow),
                            ),
                            ReportLabel::new(
                                capture_list_begin.clone(),
                                "capture list begins here".to_string(),
                                Some(Color::Green),
                            ),
                            ReportLabel::new(
                                capture.span.clone(),
                                format!("capture is here, consider placing a '{}' afterwards", ','.fg(Color::Blue)),
                                Some(Color::Blue),
                            ),
                        ],
                    );
                } else if front.token_type == Comma {
                    self.eat_any();
                }
                captures.push(capture);
            }
            let err = self.eat(RightBracket, format!("'{}' to close off the capture list for a closure", ']'.fg(Color::Green)), ExpectedCaptureClose, None, vec![
                ReportLabel::new(
                    location.clone(),
                    "closure begins here".to_string(),
                    Some(Color::Yellow),
                ),
                ReportLabel::new(
                    capture_list_begin.clone(),
                    "capture list begins here".to_string(),
                    Some(Color::Green),
                ),
            ]);
        } else {
            captures.push(AstNode::new(location.clone(), ConstReferenceImplicitCapture));
        }
        let mut return_type = None;
        let peek = self.peek_ref();
        if peek.token_type == Arrow {
            self.eat_any();
            return_type = Some(self.parse_type(true));
        }
        let body = self.parse_expression();
        AstNode::new(location.expanded(&body.span), Closure {
            arguments,
            captures,
            return_type,
            body,
        })
    }

    // A function for parsing the beginning of a function
    // Either for function types, or anonymous functions
    fn parse_function_begin(&mut self, is_for_type: bool) -> (FileSpan, NodeList, OptionalNode, DeclarationFlags) {
        let location = self.peek_ref().span.clone();
        self.eat_any();
        let mut peek = self.peek_ref();
        if peek.token_type != LeftParentheses {
            _ = self.unexpected(
                peek.clone(),
                format!("'{}' to start argument list for {}", '('.fg(Color::Green), if is_for_type { "function type".fg(Color::Red) } else { "anonymous function".fg(Color::Yellow) }),
                ExpectedOpeningParentheses,
                None,
                vec![
                    ReportLabel::new(
                        location.clone(),
                        format!("{} begins here", if is_for_type { "function type".fg(Color::Red) } else { "anonymous function".fg(Color::Yellow) }),
                        Some(if is_for_type { Color::Red } else { Color::Yellow }),
                    )
                ],
            );
        }
        let arguments = self.parse_arguments(RightParentheses, location.clone(), if is_for_type { "function type" } else { "anonymous function" }, "unnamed", "", ")", if is_for_type { Color::Red } else { Color::Yellow });
        // We should parse flags before the return type, as we might be returning a function type that could have flags
        let flags = self.parse_flags();
        let mut return_type = None;
        peek = self.peek_ref();
        if peek.token_type == Arrow {
            self.eat_any();
            return_type = Some(self.parse_type(true));
        } else if is_for_type {
            return_type = Some(self.unexpected(peek.clone(), format!("'{}' to begin return type for function type", "->".fg(Color::Green)), ExpectedReturnType, None, vec![
                ReportLabel::new(
                    location.clone(),
                    format!("{} begins here", if is_for_type { "function type".fg(Color::Red) } else { "anonymous function".fg(Color::Yellow) }),
                    Some(if is_for_type { Color::Red } else { Color::Yellow }),
                )
            ]));
        }
        (location, arguments, return_type, flags)
    }

    fn parse_anonymous_function(&mut self) -> NodePtr {
        let (location, arguments, return_type, flags) = self.parse_function_begin(false);

        let peek = self.peek_ref();
        let mut cg = ColorGenerator::new();
        match peek.token_type {
            TokenType::ThickArrow => {
                self.eat_any();
                let body = self.parse_expression();
                let end_loc = self.peek_ref().span.end.clone();
                AstNode::new(
                    FileSpan {
                        begin: location.begin,
                        end: end_loc,
                    },
                    AnonymousFunction {
                        flags,
                        arguments,
                        return_type,
                        body,
                    })
            }
            TokenType::LeftBrace => {
                let body = self.parse_expression_block();
                AstNode::new(FileSpan {
                    begin: location.begin,
                    end: body.span.end.clone(),
                }, AnonymousFunction {
                    flags,
                    arguments,
                    return_type,
                    body,
                })
            }
            _ => self.unexpected(
                peek.clone(),
                format!("'{}', '{}', following an anonymous function declaration", "=>".fg(cg.next()), "{".fg(cg.next())),
                ExpectedFunctionInformation,
                None,
                vec![
                    ReportLabel::new(
                        location.clone(),
                        "anonymous function begins here".to_string(),
                        Some(Color::Yellow),
                    )
                ],
            )
        }
    }

    fn parse_comptime(&mut self) -> NodePtr {
        todo!()
    }

    fn parse_expression_block(&mut self) -> NodePtr {
        todo!()
    }

    fn parse_named_block(&mut self) -> NodePtr {
        todo!()
    }

    fn parse_if(&mut self) -> NodePtr {
        let location = self.peek_ref().span.clone();
        self.eat_any();
        let paren_location = self.peek_ref().span.clone();
        let err = self.eat(
            LeftParentheses,
            format!("'{}' to begin the condition in an if statement", '('.fg(Color::Green)),
            ExpectedOpeningParentheses,
            Some("Cheese requires parentheses around conditions in control structures.".to_string()),
            vec![
                ReportLabel::new(
                    location.clone(),
                    format!("{} statement begins here, expected '{}' afterwards", "if".fg(Color::Blue), '('.fg(Color::Green)),
                    Some(Color::Blue),
                )
            ],
        );
        if let Some(err) = err {
            return err;
        }
        let condition = self.parse_expression();
        let mut capture = None;
        if self.peek_ref().token_type == Colon {
            self.eat_any();
            capture = Some(self.parse_capture(false));
        }
        let err = self.eat(
            RightParentheses,
            format!("'{}' to close the condition on an if statement", ')'.fg(Color::Green)),
            ExpectedMatchingParentheses,
            Some("Cheese requires parentheses around conditions in control structures.".to_string()),
            vec![
                ReportLabel::new(
                    paren_location.clone(),
                    format!("'{}' found here, expected matching '{}'", '('.fg(Color::Green), ')'.fg(Color::Green)),
                    Some(Color::Green),
                )
            ],
        );
        if let Some(err) = err {
            return err;
        }
        let body = self.parse_expression();
        let mut els = None;
        if self.peek_ref().token_type == Else {
            self.eat_any();
            els = Some(self.parse_expression())
        }
        AstNode::new(
            location.expanded(if let Some(x) = &els {
                &x.span
            } else {
                &body.span
            }),
            If {
                condition,
                unwrap: capture,
                body,
                else_statement: els,
            },
        )
    }


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
            ConstantPointer => {
                self.eat_any();
                capture_type = CaptureType::Const;
            }
            Identifier | TokenType::Underscore => {}
            _ => return self.unexpected(front.clone(), format!("'{}', '{}', '{}', or an identifier for a capture", '*'.fg(Color::Green), "*~".fg(Color::Green), '='.fg(Color::Green)), ExpectedCaptureSpecifier, None, vec![])
        }
        let front = self.peek();
        if front.token_type == Identifier || front.token_type == TokenType::Underscore {
            let end = front.span.clone();
            let name = front.value;
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
            self.unexpected(front, "a name for a capture in a context disallowing implicit captures".to_string(), ExpectedCaptureName, Some("Cheese only allows implicit captures in closures.".to_string()), vec![
                ReportLabel::new(
                    location,
                    "capture specifier begins here, expected name afterwards".to_string(),
                    Some(Color::Green),
                )
            ])
        }
    }


    fn parse_match(&mut self) -> NodePtr {
        todo!()
    }

    fn parse_for(&mut self) -> NodePtr {
        todo!()
    }

    fn parse_while(&mut self) -> NodePtr {
        todo!()
    }

    fn parse_loop(&mut self) -> NodePtr {
        todo!()
    }
}