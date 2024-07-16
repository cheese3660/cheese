use ariadne::{Color, Fmt};
use cheese_diagnostics::ErrorCode::{ExpectedCloseParentheses, ExpectedFn, ExpectedIdentifierOrOpeningDiamond, ExpectedOpenParentheses, ExpectedSliceTypeClose, ExpectedType};
use cheese_diagnostics::locating::FileSpan;
use cheese_diagnostics::ReportLabel;
use cheese_lexer::{Token, TokenType};
use cheese_lexer::TokenType::{Arrow, ConstantArray, ConstantPointer, Fn, GreaterThan, LeftParentheses, RightBracket, RightParentheses};
use crate::ast::{AstNode, NodeList, NodePtr, OptionalNode};
use crate::ast::AstNodeData::{ArrayType, Bool, BuiltinReference, Combine, CompileTimeComplex, CompileTimeFloat, CompileTimeInteger, CompileTimeString, Complex32, Complex64, Float32, Float64, FunctionInterfaceType, FunctionTraitType, FunctionType, GenericInstanceReference, ImplicitArray, InferredSize, MutableFunctionInterfaceType, MutableFunctionTraitType, Opaque, Reference, SelfType, SignedIntegerType, SignedSize, Slice, Tuple, Type, TypeMemberReference, TypeName, UnknownSize, UnsignedIntegerType, UnsignedSize, Void};
use crate::parser::{ExpectedInformation, NO_NOTE, Parser};
use crate::try_err;
use crate::utilities::{builtin_remove_prefix, get_integer_type_size, is_flag};

impl Parser {
    pub(super) fn parse_type(&mut self) -> NodePtr {
        self.parse_type_secondary(Self::parse_type_primary)
    }

    pub(super) fn parse_generic_call_list(&mut self) -> (NodeList, FileSpan) {
        let (a,b,c) = self.parse_comma_separated_list(Self::parse_generic_type_argument, |t| t == GreaterThan, ">");
        (a,b)
    }

    pub(super) fn parse_type_common(&mut self, start: Token, start_loc: FileSpan, start_location: usize) -> OptionalNode {
        match start.token_type {
            TokenType::Identifier => Some(AstNode::new(start_loc, TypeName(start.value))),
            TokenType::BuiltinReference => Some(AstNode::new(start_loc, BuiltinReference(builtin_remove_prefix(start.value)))),
            TokenType::Float32 => Some(AstNode::new(start_loc, Float32)),
            TokenType::Float64 => Some(AstNode::new(start_loc, Float64)),
            TokenType::Complex32 => Some(AstNode::new(start_loc, Complex32)),
            TokenType::Complex64 => Some(AstNode::new(start_loc, Complex64)),
            TokenType::Type => Some(AstNode::new(start_loc, Type)),
            TokenType::Void => Some(AstNode::new(start_loc, Void)),
            TokenType::CompileTimeString => Some(AstNode::new(start_loc, CompileTimeString)),
            TokenType::CompileTimeFloat => Some(AstNode::new(start_loc, CompileTimeFloat)),
            TokenType::CompileTimeComplex => Some(AstNode::new(start_loc, CompileTimeComplex)),
            TokenType::CompileTimeInt => Some(AstNode::new(start_loc, CompileTimeInteger)),
            TokenType::UnsignedSize => Some(AstNode::new(start_loc, UnsignedSize)),
            TokenType::SignedSize => Some(AstNode::new(start_loc, SignedSize)),
            TokenType::Opaque => Some(AstNode::new(start_loc, Opaque)),
            TokenType::TypeSelf => Some(AstNode::new(start_loc, SelfType)),
            TokenType::UnsignedIntegerType => Some(AstNode::new(start_loc, UnsignedIntegerType(get_integer_type_size(start.value)))),
            TokenType::SignedIntegerType => Some(AstNode::new(start_loc, SignedIntegerType(get_integer_type_size(start.value)))),
            TokenType::Star | ConstantPointer => Some({
                let subtype = self.parse_type();
                AstNode::new(start_loc.expanded(&subtype.span),Reference {constant: start.token_type == ConstantPointer, subtype})
            }),
            TokenType::Bool => Some(AstNode::new(start_loc, Bool)),
            TokenType::NoReturn => Some(AstNode::new(start_loc, Bool)),
            TokenType::LeftBracket => Some(self.parse_array_type(start_loc)),
            TokenType::LeftParentheses => Some(self.parse_tuple_type(start_loc)),
            Fn => {
                self.location = start_location;
                Some(self.parse_fn_pointer_type())
            }
            TokenType::FnTrait => Some(self.parse_fn_trait_type(start_loc)),
            TokenType::FnMutTrait => Some(self.parse_fn_mut_trait_type(start_loc)),
            TokenType::FnInterface => Some(self.parse_fn_interface_type(start_loc)),
            TokenType::FnMutInterface => Some(self.parse_fn_mut_interface_type(start_loc)),
            t => {
                if is_flag(t) {
                    self.location = start_location;
                    Some(self.parse_fn_pointer_type())
                } else {
                    None
                }
            }
        }
    }


    fn parse_type_secondary<T: std::ops::Fn(&mut Parser) -> NodePtr>(&mut self, parse_primary_method: T) -> NodePtr {
        let mut base = parse_primary_method(self);
        loop {
            let peek = self.peek_ref();
            let loc = peek.span.clone();
            match peek.token_type {
                TokenType::DoubleColon => {
                    self.eat_any();
                    let peek2 = self.peek_ref();
                    let loc2 = peek2.span.clone();
                    match peek2.token_type {
                        TokenType::LessThan => {
                            self.eat_any();
                            let (args, end) = self.parse_generic_call_list();
                            base = AstNode::new(base.span.expanded(&end), GenericInstanceReference {
                                referee: base,
                                generic_args: args,
                            })
                        },
                        TokenType::Identifier => {
                            let name = peek2.value.clone();
                            self.eat_any();
                            base = AstNode::new(base.span.expanded(&loc2), TypeMemberReference {
                                referee: base,
                                member: name
                            })
                        },
                        _ => return self.unexpected(peek2.clone(), format!("an identifier or '{}'", '<'.fg(Color::Green)), ExpectedIdentifierOrOpeningDiamond, NO_NOTE, vec![
                            ReportLabel::new(
                                loc,
                                format!("expected identifier or '{}' after this '{}'", '<'.fg(Color::Green), "::".fg(Color::Green)),
                                Some(Color::Green),
                            )
                        ])
                    }
                },
                TokenType::LessThan => {
                    self.eat_any();
                    let (args, end) = self.parse_generic_call_list();
                    base = AstNode::new(base.span.expanded(&end), GenericInstanceReference {
                        referee: base,
                        generic_args: args,
                    })
                },
                TokenType::Ampersand => {
                    self.eat_any();
                    let combined = self.parse_type();
                    base = AstNode::new(FileSpan {
                        begin: base.span.begin.clone(),
                        end: combined.span.end.clone(),
                    }, Combine {
                        lhs: base,
                        rhs: combined,
                    })
                },
                _ => break
            }
        }
        base
    }

    fn parse_type_primary(&mut self) -> NodePtr {
        let old_loc = self.location;
        let start = self.peek();
        let start_loc = start.span.clone();
        self.eat_any();
        match self.parse_type_common(start, start_loc, old_loc) {
            Some(v) => v,
            _ => {
                self.location = old_loc;
                self.unexpected(self.peek(), "a type", ExpectedType, NO_NOTE, vec![])
            }
        }
    }
    pub(super) fn parse_generic_type_argument(&mut self) -> NodePtr {
        self.parse_type_secondary(Self::parse_generic_arg_primary)
    }


    fn parse_generic_arg_primary(&mut self) -> NodePtr {
        let old_loc = self.location;
        let start = self.peek();
        let start_loc = start.span.clone();
        self.eat_any();
        match self.parse_type_common(start, start_loc, old_loc) {
            Some(v) => v,
            _ => {
                self.location = old_loc;
                self.parse_expression()
            }
        }
    }

    fn parse_array_type(&mut self, start_loc: FileSpan) -> NodePtr {
        let (dimensions, _, end_token) = self.parse_comma_separated_list(Self::parse_array_argument, |t| t == RightBracket || t == ConstantArray, "](~)");
        let constant = end_token == ConstantArray;
        let subtype = self.parse_type();
        AstNode::new(start_loc.expanded(&subtype.span), if dimensions.len() == 0 {
            ImplicitArray {
                constant,
                subtype
            }
        } else {
            ArrayType {
                constant,
                dimensions,
                child: subtype
            }
        })
    }

    fn parse_array_argument(&mut self) -> NodePtr {
        match self.peek_ref().token_type {
            TokenType::Underscore => {
                self.eat_any();
                AstNode::new(self.peek_ref().span.clone(), InferredSize)
            },
            TokenType::Question => {
                self.eat_any();
                AstNode::new(self.peek_ref().span.clone(), UnknownSize)
            },
            _ => self.parse_expression()
        }
    }

    fn parse_slice_type(&mut self, start_loc: FileSpan) -> NodePtr {
        let constant = match self.peek_ref().token_type {
            TokenType::GreaterThan => false,
            TokenType::ConstantSlice => true,
            _ => return self.unexpected(
                self.peek(),
                format!("'{}' or '{}' to denote a slice type", '>'.fg(Color::Green), ">~".fg(Color::Green)),
                ExpectedSliceTypeClose,
                NO_NOTE,
                vec![
                    ReportLabel::new(
                        start_loc,
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
        let subtype = self.parse_type();
        AstNode::new(
            start_loc.expanded(&subtype.span),
            Slice {
                constant,
                subtype
            }
        )
    }


    fn parse_tuple_type_list(&mut self) -> (NodeList, FileSpan) {
        let (a,b,_) = self.parse_comma_separated_list(Self::parse_type, |t| t == RightParentheses, ")");
        (a, b)
    }

    fn parse_tuple_type(&mut self, start_loc: FileSpan) -> NodePtr {
        let (types, end_loc) = self.parse_tuple_type_list();
        AstNode::new(start_loc.expanded(&end_loc), Tuple(types))
    }

    fn parse_fn_argument_data(&mut self) -> (NodeList, NodePtr) {
        let mut result = vec![];
        try_err!(self.eat(LeftParentheses, |_| ExpectedInformation::new(
            "a '(' to begin a functional type argument list",
            ExpectedOpenParentheses,
            NO_NOTE,
            vec![]
        )),result);
        let (mut result2, close_location, _) = self.parse_comma_separated_list(Parser::parse_type, |t| t == RightParentheses, ")");
        result.append(&mut result2);
        let rt = if self.peek_ref().token_type == Arrow {
            self.eat_any();
            self.parse_type()
        } else {
            AstNode::new(close_location,Void)
        };
        (result, rt)
    }

    fn parse_fn_pointer_type(&mut self) -> NodePtr {
        let start_loc = self.peek_ref().span.clone();
        let flags = self.parse_flags();
        try_err!(self.eat(Fn, |tt| ExpectedInformation::new(
            format!("'{}' to begin a function pointer type","fn".fg(Color::Blue)),
            ExpectedFn,
            match tt {
                TokenType::FnTrait | TokenType::FnMutTrait | TokenType::FnInterface | TokenType::FnMutInterface => Some("function trait/interface syntactic sugar keywords cannot have flags attached to them!"),
                _ => NO_NOTE
            },
            vec![
                ReportLabel::new(
                    start_loc.clone(),
                    "function pointer type begins here!",
                    Some(Color::Red)
                )
            ]
        )));
        let (args, rt) = self.parse_fn_argument_data();
        AstNode::new(start_loc.expanded(&rt.span), FunctionType {
            flags,
            arguments: args,
            return_type: rt
        })
    }

    fn parse_fn_trait_type(&mut self, start_loc: FileSpan) -> NodePtr {
        let (args, rt) = self.parse_fn_argument_data();
        AstNode::new(start_loc.expanded(&rt.span), FunctionTraitType {
            arguments: args,
            return_type: rt
        })
    }
    fn parse_fn_mut_trait_type(&mut self, start_loc: FileSpan) -> NodePtr {
        let (args, rt) = self.parse_fn_argument_data();
        AstNode::new(start_loc.expanded(&rt.span), MutableFunctionTraitType {
            arguments: args,
            return_type: rt
        })
    }
    fn parse_fn_interface_type(&mut self, start_loc: FileSpan) -> NodePtr {
        let (args, rt) = self.parse_fn_argument_data();
        AstNode::new(start_loc.expanded(&rt.span), FunctionInterfaceType {
            arguments: args,
            return_type: rt
        })
    }
    fn parse_fn_mut_interface_type(&mut self, start_loc: FileSpan) -> NodePtr {
        let (args, rt) = self.parse_fn_argument_data();
        AstNode::new(start_loc.expanded(&rt.span), MutableFunctionInterfaceType {
            arguments: args,
            return_type: rt
        })
    }
}