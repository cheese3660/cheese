use ariadne::{Color, ColorGenerator, Fmt};
use num_bigint::BigInt;
use cheese_diagnostics::ErrorCode::{ExpectedArgumentClose, ExpectedClose, ExpectedCloseParentheses, ExpectedColon, ExpectedCommaOrClose, ExpectedEnumId, ExpectedExpressionName, ExpectedFieldName, ExpectedIdentifierOrOpeningDiamond, ExpectedName, ExpectedOpenParentheses, ExpectedPrimary, ExpectedSemicolon, InvalidCharacterLiteral};
use cheese_diagnostics::locating::{File, FileSpan};
use cheese_diagnostics::ReportLabel;
use cheese_lexer::TokenType;
use cheese_lexer::TokenType::{Colon, Comma, Else, Identifier, LeftParentheses, RightBrace, RightBracket, RightParentheses, Semicolon};
use cheese_utilities::strings::Escapable;
use crate::ast::{AstNode, AstNodeData, NodeList, NodePtr, OptionalNode};
use crate::ast::AstNodeData::{AddressOf, ArrayCall, ArrayLiteral, Block, Break, BuiltinReference, Continue, Dereference, EmptyReturn, EnumLiteral, False, FieldLiteral, FilterTransformation, FloatLiteral, For, GenericInstanceReference, If, ImaginaryLiteral, ImplicitResult, IntegerLiteral, Loop, MapTransformation, NamedBreak, NamedExpression, NamedYield, NameReference, Not, ObjectCall, ObjectLiteral, Return, SelfValue, StringLiteral, Subscription, True, TupleCall, TupleLiteral, TypeMemberReference, Typeof, UnaryMinus, UnaryPlus, Underscore, While, Yield};
use crate::ast::Operator::Assign;
use crate::parser::{ExpectedInformation, NO_NOTE, Parser};
use crate::try_err;
use crate::utilities::{bin_to_big_int, builtin_remove_prefix, create_node_from_binop, dec_to_big_int, hex_to_big_int, is_binary_operand_type, is_binary_operation, is_statement_end, oct_to_big_int, precedence};

impl Parser {
    pub(super) fn parse_expression(&mut self) -> NodePtr {
        let primary = self.parse_primary(None);
        self.parse_expression_precedence(primary,0)
    }

    fn parse_expression_precedence(&mut self, mut lhs: NodePtr, min_precedence: u8) -> NodePtr {
        let mut lookahead = self.peek_ref().token_type;
        while is_binary_operation(lookahead) && precedence(lookahead) >= min_precedence {
            let op = lookahead;
            let op_precedence = precedence(lookahead);
            self.eat_any();
            let mut rhs = if is_binary_operand_type(op) {
                self.parse_type()
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
        self.eat_any();
        let (result, end_location, _) = self.parse_comma_separated_list(Self::parse_expression, |t| t == ending_type, ending_str);
        (result, end_location)
    }

    fn parse_object_list(&mut self) -> (NodeList, FileSpan) {
        let opening_location = self.peek_ref().span.clone();
        self.eat_any();
        let mut result = vec![];
        while match self.peek_ref().token_type {
            RightBrace | TokenType::EndOfFile => false,
            _ => true
        } {
            let name = self.peek_ref().value.clone();
            let name_loc = self.peek_ref().span.clone();
            try_err!(self.eat(Identifier, |_| ExpectedInformation::new("a field name for a field in the field list for an object literal", ExpectedFieldName, NO_NOTE, vec![
                ReportLabel::new(
                    opening_location.clone(),
                    "field list begins here",
                    Some(Color::Cyan),
                )
            ])),result);
            let front_ty = self.peek_ref().token_type;
            let argument = if front_ty == Comma || front_ty == RightBrace {
                AstNode::new(
                    name_loc.clone(),
                    FieldLiteral {
                        value: AstNode::new(
                            name_loc,
                            NameReference(name.clone())
                        ),
                        name
                    }
                )
            } else {
                try_err!(self.eat(Colon, |_| ExpectedInformation::new(format!("a '{}', '{}', or '{}' following a field name for a field in the field list for an object literal", ':'.fg(Color::Green), ','.fg(Color::Green), '}'.fg(Color::Green)), ExpectedColon,
                    NO_NOTE, vec![
                    ReportLabel::new(
                        opening_location.clone(),
                        "field list begins here",
                        Some(Color::Cyan),
                    ),
                    ReportLabel::new(
                        name_loc.clone(),
                        format!("field begins here, expected '{}' after this", ':'.fg(Color::Green)),
                        Some(Color::Yellow),
                    ),
                ])),result);
                let value = self.parse_expression();
                AstNode::new(
                    name_loc.expanded(&value.span),
                    FieldLiteral {
                        name,
                        value,
                    },
                )
            };
            let peek_tok = self.peek_ref();
            if peek_tok.token_type != Comma && peek_tok.token_type != RightBrace {
                let clone = peek_tok.clone();
                _ = self.unexpected(clone.clone(), format!("'{}' or '{}' following a field in the field list for an object literal", ','.fg(Color::Green), '}'.fg(Color::Green)), ExpectedCommaOrClose, NO_NOTE, vec![
                    ReportLabel::new(
                        opening_location.clone(),
                        "argument list begins here",
                        Some(Color::Cyan),
                    ),
                    ReportLabel::new(
                        argument.span.clone(),
                        format!("expected '{}' or '{}' following this field, consider inserting either", ','.fg(Color::Green), '}'.fg(Color::Green)),
                        Some(Color::Yellow),
                    ),
                ]);
                if is_statement_end(clone.token_type) {
                    break;
                }
            } else if peek_tok.token_type == Comma {
                self.eat_any();
            }
            result.push(argument);
        }
        let end_location = self.peek_ref().span.clone();try_err!(self.eat(RightBrace, |_| ExpectedInformation::new(format!("'{}' to end the field list for an object literal", '}'.fg(Color::Green)), ExpectedArgumentClose, NO_NOTE, vec![
            ReportLabel::new(
                opening_location.clone(),
                "field list begins here",
                Some(Color::Cyan),
            )])),result);
        (result, end_location)
    }

    pub(super) fn parse_primary(&mut self, lookbehind: OptionalNode) -> NodePtr {
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
                        rhs: next
                    })
                }
                TokenType::LeftParentheses => {
                    let (args, end_span) = self.parse_call_list(RightParentheses, ")");
                    base = AstNode::new(base.span.expanded(&end_span), TupleCall {
                        functional: base,
                        args
                    })
                },
                TokenType::LeftBracket => {
                    let (args, end_span) = self.parse_call_list(RightBracket, "]");
                    base = AstNode::new(base.span.expanded(&end_span), ArrayCall {
                        functional: base,
                        args
                    })
                }
                TokenType::LeftBrace => {
                    let (args, end_span) = self.parse_object_list();
                    base = AstNode::new(base.span.expanded(&end_span), ObjectCall {
                        functional: base,
                        args
                    })
                }
                TokenType::DoubleColon => {
                    self.eat_any();
                    let peek2 = self.peek_ref();
                    let loc2 = peek2.span.clone();
                    match peek2.token_type {
                        TokenType::LessThan => {
                            self.eat_any();
                            let (args, end_span) = self.parse_generic_call_list();
                            base = AstNode::new(base.span.expanded(&end_span), GenericInstanceReference {
                                referee: base,
                                generic_args: args,
                            })
                        },
                        Identifier => {
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
                }
                TokenType::Dereference => {
                    base = AstNode::new(base.span.expanded(&front.span),Dereference(base));
                    self.eat_any();
                }
                _ => break
            }
        }
        base
    }

    fn parse_unary<T: Fn(NodePtr) -> AstNodeData>(&mut self, start_loc: FileSpan, builder: T) -> NodePtr {
        let rest = self.parse_expression();
        AstNode::new(start_loc.expanded(&rest.span), builder(rest))
    }

    fn parse_expression_or_tuple(&mut self) -> (NodeList, FileSpan, bool) {
        let mut result = vec![];
        let mut last_was_comma = false;
        while match self.peek_ref().token_type {
            TokenType::EndOfFile | TokenType::RightParentheses => false,
            _ => true
        } {
            last_was_comma = false;
            let current = self.parse_expression();
            if match self.peek_ref().token_type {
                Comma  | TokenType::RightParentheses => false,
                _ => true
            } {
                result.push(self.unexpected(self.peek(), "a ',' or ')' following a value in a parenthetical expression", ExpectedCommaOrClose, NO_NOTE, vec![
                    ReportLabel::new(
                        current.span.clone(),
                        "expected after this value",
                        None
                    )
                ]))
            }
            if self.peek_ref().token_type == Comma {
                last_was_comma = true;
                self.eat_any()
            }
            result.push(current);
        }
        let end_loc = self.peek_ref().span.clone();
        let end_type = self.peek_ref().token_type;
        if end_type != RightParentheses {
            result.push(self.unexpected(self.peek(), "expected ')' to close parenthetical expression", ExpectedClose, NO_NOTE, vec![]));
        }
        let len = result.len();
        (result, end_loc, last_was_comma || len != 1)
    }

    fn parse_expression_name(&mut self) -> Option<String> {
        let start_loc = self.peek_ref().span.clone();
        if self.peek_ref().token_type == Colon {
            self.eat_any();
            let name = self.peek_ref().value.clone();
            _ = self.eat(Identifier,|_| ExpectedInformation::new("Expected a name for a named expression", ExpectedExpressionName, NO_NOTE, vec![
                ReportLabel::new(
                    start_loc.clone(),
                    "expected after ':' here",
                    None
                ),
            ]));
            Some(name)
        } else {
            None
        }
    }

    pub(super) fn parse_primary_base(&mut self) -> NodePtr {
        let start = self.peek();
        let return_to = self.location;
        let start_loc = start.span.clone();
        self.eat_any();
        let mut go_back = || self.location = return_to;
        let mut cg = ColorGenerator::new();
        match start.token_type {
            TokenType::DecimalLiteral => AstNode::new(start_loc, IntegerLiteral(dec_to_big_int(start.value))),
            TokenType::HexadecimalLiteral => AstNode::new(start_loc, IntegerLiteral(hex_to_big_int(start.value))),
            TokenType::OctalLiteral => AstNode::new(start_loc, IntegerLiteral(oct_to_big_int(start.value))),
            TokenType::BinaryLiteral => AstNode::new(start_loc, IntegerLiteral(bin_to_big_int(start.value))),
            TokenType::FloatingLiteral => AstNode::new(start_loc, FloatLiteral(start.value.replace("_", "").parse().unwrap())),
            TokenType::ImaginaryLiteral => AstNode::new(start_loc, ImaginaryLiteral(start.value.replace("_", "").replace("I", "").parse().unwrap())),
            Identifier => AstNode::new(start_loc, NameReference(start.value)),
            TokenType::BuiltinReference => AstNode::new(start_loc, BuiltinReference(builtin_remove_prefix(start.value))),
            TokenType::StringLiteral => AstNode::new(start_loc, StringLiteral(start.value.unescape_with_quotes())),
            TokenType::CharacterLiteral => {
                let unescaped = start.value.unescape_with_quotes();
                let len = unescaped.chars().count();
                if len != 1 {
                    return self.raise(start_loc.clone(), start_loc.begin.clone(), InvalidCharacterLiteral, format!("invalid length for character literal: {len}, character literals must have 1 character"), NO_NOTE, vec![
                        ReportLabel::new(start_loc.clone(), "invalid character literal found here", Some(Color::Red))
                    ]);
                }
                let first = unescaped.chars().next().unwrap();
                AstNode::new(start_loc, IntegerLiteral(BigInt::from(first as u32)))
            },
            TokenType::Ampersand => self.parse_unary(start_loc, |rest| AddressOf(rest)),
            TokenType::Dash => self.parse_unary(start_loc, |rest| UnaryMinus(rest)),
            TokenType::Plus => self.parse_unary(start_loc, |rest| UnaryPlus(rest)),
            TokenType::Not => self.parse_unary(start_loc, |rest| Not(rest)),
            TokenType::True => AstNode::new(start_loc, True),
            TokenType::False => AstNode::new(start_loc, False),
            TokenType::None => AstNode::new(start_loc, AstNodeData::None),
            TokenType::Pipe => {
                go_back();
                self.parse_closure()
            },
            TokenType::Fn => {
                go_back();
                self.parse_anonymous_function()
            },
            TokenType::Tuple => {
                let (args, end, _) = self.parse_comma_separated_list(Self::parse_expression, |t| t == RightParentheses, ")");
                AstNode::new(start_loc.expanded(&end), TupleLiteral(args))
            }
            TokenType::Array => {
                let (args, end, _) = self.parse_comma_separated_list(Self::parse_expression, |t| t == RightBracket, "]");
                AstNode::new(start_loc.expanded(&end), ArrayLiteral(args))
            }
            TokenType::LeftBracket => {
                let (args, end, _) = self.parse_comma_separated_list(Self::parse_expression, |t| t == RightBracket, "]");
                AstNode::new(start_loc.expanded(&end), ArrayLiteral(args))
            }
            TokenType::Object => {
                go_back();
                let (args, end) = self.parse_object_list();
                AstNode::new(start_loc.expanded(&end), ObjectLiteral(args))
            }
            TokenType::LeftParentheses => {
                let (mut args, end, is_tuple) = self.parse_expression_or_tuple();
                if is_tuple {
                    AstNode::new(start_loc.expanded(&end), TupleLiteral(args))
                } else {
                    args.remove(0)
                }
            }
            Colon => {
                let name = self.peek_ref().value.clone();
                self.eat(Identifier,|_| ExpectedInformation::new("Expected a name for a named expression", ExpectedExpressionName, NO_NOTE, vec![
                    ReportLabel::new(
                        start_loc.clone(),
                        "expected after ':' here",
                        None
                    ),
                ])).unwrap_or_else(|| {
                    let expr = self.parse_expression();
                    AstNode::new(start_loc.expanded(&expr.span), NamedExpression {
                        name,
                        expr
                    })
                })
            },
            TokenType::Break => {
                let name = self.parse_expression_name();
                match name {
                    Some(name) => {
                        AstNode::new(start_loc, NamedBreak(name))
                    },
                    None => AstNode::new(start_loc, Break)
                }
            },
            TokenType::Yield => {
                let name = self.parse_expression_name();
                let value = self.parse_expression();
                AstNode::new(start_loc.expanded(&value.span),match name {
                    Some(name) => NamedYield {
                        name,
                        value
                    },
                    None => Yield(value)
                })
            }
            TokenType::Dot => {
                let id = self.peek_ref().value.clone();
                let loc = self.peek_ref().span.clone();
                self.eat(Identifier, |_| ExpectedInformation::new("an identifier for an enum literal", ExpectedEnumId, NO_NOTE, vec![
                    ReportLabel::new(
                        start_loc.clone(),
                        format!("expected identifier after this '{}'", '.'.fg(Color::Green)),
                        Some(Color::Green),
                    )
                ])).unwrap_or_else(|| AstNode::new(start_loc.expanded(&loc), EnumLiteral(id)))
            }
            TokenType::LeftBrace => {
                go_back();
                self.parse_expression_block()
            }
            TokenType::Return => if !is_statement_end(self.peek_ref().token_type) {
                let value = self.parse_expression();
                AstNode::new(start_loc.expanded(&value.span), Return(value))
            } else {
                AstNode::new(start_loc, EmptyReturn)
            },
            TokenType::Continue => AstNode::new(start_loc, Continue),
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
            TokenType::SelfType => AstNode::new(start_loc, SelfValue),
            TokenType::Underscore => AstNode::new(start_loc, Underscore),
            TokenType::Typeof => {
                let result = self.parse_type();
                AstNode::new(start_loc.expanded(&result.span),Typeof(result))
            }
            _ => {
                match self.parse_type_common(start.clone(),start_loc,return_to) {
                    Some(t) => t,
                    None => {
                        if is_statement_end(start.token_type) {
                            self.location = return_to;
                        }
                        self.unexpected(start, format!(
                            "any of the following: {}, {}, {}, {}, {}, {}, {}, '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', or a type.",
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
                            '['.fg(cg.next()),
                            "return".fg(cg.next()),
                            "continue".fg(cg.next()),
                            "break".fg(cg.next()),
                            "yield".fg(cg.next()),
                            "comptime".fg(cg.next()),
                            ":".fg(cg.next()),
                            "if".fg(cg.next()),
                            "match".fg(cg.next()),
                            "loop".fg(cg.next()),
                            "self".fg(cg.next()),
                            "for".fg(cg.next()),
                            "while".fg(cg.next()),
                            "typeof".fg(cg.next()),
                            '_'.fg(cg.next())
                        ), ExpectedPrimary, NO_NOTE, vec![])
                    }
                }
            }
        }
    }
    fn parse_expression_block(&mut self) -> NodePtr {
        let (children, span) = self.parse_expression_block_list();
        AstNode::new(span,Block(children))
    }

    pub(super)
    fn parse_expression_block_list(&mut self) -> (NodeList,FileSpan) {
        let block_start = self.peek_ref().span.clone();
        let mut children = vec![];
        self.eat_any();
        while match self.peek_ref().token_type {
            TokenType::EndOfFile | RightBrace => false,
            _ => true
        } {
            if self.peek_ref().token_type == Semicolon {
                self.eat_any();
                continue;
            }
            let (child, require_semi) = self.parse_block_statement();
            match self.peek_ref().token_type {
                Semicolon => {
                    self.eat_any();
                    children.push(child);
                },
                RightBrace => {
                    children.push(AstNode::new(child.span.clone(), ImplicitResult(child)));
                },
                _ => {
                    if require_semi {
                        children.push(self.unexpected(self.peek(),"a ';' or '}' after a statement that requires a semicolon in a block", ExpectedSemicolon, NO_NOTE, vec![
                            ReportLabel::new(
                                child.span.clone(),
                                "expected after this statement",
                                None
                            )
                        ]))
                    }
                    children.push(child);
                }
            }
        }
        let last_location = self.peek_ref().span.clone();
        try_err!(self.eat(RightBrace, |_|ExpectedInformation::new(
            "Expected '}' to end block!",
            ExpectedClose,
            NO_NOTE,
            vec![
                ReportLabel::new(
                    block_start.clone(),
                    "block starts here",
                    None
                )
            ]
        )),children);
        (children, block_start.expanded(&last_location))
    }

    // The bool part is if it requires a semicolon
    fn parse_block_statement(&mut self) -> (NodePtr, bool) {
        match self.peek_ref().token_type {
            TokenType::Let => (self.parse_let(), true),
            TokenType::Fn => self.parse_local_or_anonymous_function(),
            TokenType::If => (self.parse_if(), false),
            TokenType::Match => (self.parse_match(), false),
            TokenType::For => (self.parse_for(), false),
            TokenType::While => (self.parse_while(), false),
            TokenType::Loop => (self.parse_loop(), false),

            _ => (self.parse_expression(), true)
        }
    }

    fn parse_if(&mut self) -> NodePtr {
        let location = self.peek_ref().span.clone();
        self.eat_any();
        let paren_location = self.peek_ref().span.clone();
        try_err!(self.eat(
            LeftParentheses,
            |_| ExpectedInformation::new(format!("'{}' to begin the condition in an if statement", '('.fg(Color::Green)),
                                        ExpectedOpenParentheses,
                                        Some("Cheese requires parentheses around conditions in control structures."),
                                        vec![
                                            ReportLabel::new(
                                                location.clone(),
                                                format!("{} statement begins here, expected '{}' afterwards", "if".fg(Color::Blue), '('.fg(Color::Green)),
                                                Some(Color::Blue),
                                            )
                                        ],
            )));
        let condition = self.parse_expression();
        let mut capture = None;
        if self.peek_ref().token_type == Colon {
            self.eat_any();
            capture = Some(self.parse_capture(false));
        }
        try_err!(self.eat(
            RightParentheses,
            |_| ExpectedInformation::new(format!("'{}' to close the condition on an if statement", ')'.fg(Color::Green)),
                                        ExpectedCloseParentheses,
                                        Some("Cheese requires parentheses around conditions in control structures."),
                                        vec![
                                            ReportLabel::new(
                                                paren_location.clone(),
                                                format!("'{}' found here, expected matching '{}'", '('.fg(Color::Green), ')'.fg(Color::Green)),
                                                Some(Color::Green),
                                            )
                                        ],
            )));
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


    fn parse_for(&mut self) -> NodePtr {
        let location = self.peek_ref().span.clone();
        self.eat_any();
        let open_location = location.clone();
        try_err!(self.eat(
            LeftParentheses,
            |_| ExpectedInformation::new(format!("'{}' to begin the control in a for loop", '('.fg(Color::Green)),
                 ExpectedOpenParentheses,
                 Some("Cheese requires parentheses around conditions in control structures."),
                 vec![
                     ReportLabel::new(
                         location.clone(),
                         format!("{} loop begins here, expected '{}' afterwards", "for".fg(Color::Blue), '('.fg(Color::Green)),
                         Some(Color::Blue),
                     )
                 ],
            )));
        let capture = self.parse_capture(false);
        let index = if self.peek_ref().token_type == Comma {
            let comma_loc = self.peek_ref().span.clone();
            self.eat_any();
            let result = Some(AstNode::new(self.peek_ref().span.clone(),NameReference(self.peek_ref().value.clone())));
            if self.peek_ref().token_type != Identifier && self.peek_ref().token_type != TokenType::Underscore {
                Some(self.unexpected(self.peek(),format!("an identifier or '{}' following a comma in a for loop",'_'.fg(Color::Green)),ExpectedName,NO_NOTE,vec![
                    ReportLabel::new(
                        location.clone(),
                        format!("{} loop begins here","for".fg(Color::Blue)),
                        Some(Color::Blue)
                    ),
                    ReportLabel::new(
                        comma_loc.clone(),
                        format!("'{}' found here, expected name afterwards",','.fg(Color::Green)),
                        Some(Color::Green)
                    )
                ]))
            } else {
                self.eat_any();
                result
            }
        } else {
            None
        };

        try_err!(self.eat(
            Colon,
            |_| ExpectedInformation::new(
                format!("'{}' to separate the captures and the iterable in a for loop",':'.fg(Color::Green)),
                ExpectedColon,
                NO_NOTE,
                vec![
                    ReportLabel::new(
                        location.clone(),
                        "for loop begins here",
                        Some(Color::Blue)
                    )
                ]

            )
        ));
        let iterable = self.parse_expression();
        let mut transformations = vec![];
        while match self.peek_ref().token_type {
            Colon | TokenType::Question => true,
            _ => false
        } {
            let location = self.peek_ref().span.clone();
            let is_map = self.peek_ref().token_type == Colon;
            self.eat_any();
            let transform = self.parse_expression();
            transformations.push(AstNode::new(location.expanded(&transform.span),if is_map {
                MapTransformation(transform)
            } else {
                FilterTransformation(transform)
            }));
        }
        try_err!(self.eat(
            RightParentheses,
            |_| ExpectedInformation::new(format!("'{}' to close the condition on a for statement", ')'.fg(Color::Green)),
                 ExpectedCloseParentheses,
                 Some("Cheese requires parentheses around conditions in control structures."),
                 vec![
                     ReportLabel::new(
                         open_location.clone(),
                         format!("'{}' found here, expected matching '{}'", '('.fg(Color::Green), ')'.fg(Color::Green)),
                         Some(Color::Green),
                     )
                 ],
            )));
        let body = self.parse_expression();
        let els = if self.peek_ref().token_type == Else {
            self.eat_any();
            Some(self.parse_expression())
        } else {
            None
        };
        AstNode::new(
            location.expanded(match &els {
                None => &body.span,
                Some(els) => &els.span
            }),
            For {
                capture,
                index_capture: index,
                iterable,
                transformations,
                body,
                else_statement: els,
            }
        )
    }

    fn parse_while(&mut self) -> NodePtr {
        let location = self.peek_ref().span.clone();
        self.eat_any();
        let paren_location = self.peek_ref().span.clone();
        try_err!(self.eat(
            LeftParentheses,
            |_| ExpectedInformation::new(format!("'{}' to begin the condition in a while statement", '('.fg(Color::Green)),
                 ExpectedOpenParentheses,
                 Some("Cheese requires parentheses around conditions in control structures."),
                 vec![
                     ReportLabel::new(
                         location.clone(),
                         format!("{} statement begins here, expected '{}' afterwards", "while".fg(Color::Blue), '('.fg(Color::Green)),
                         Some(Color::Blue),
                     )
                 ],
            )));
        let condition = self.parse_expression();
        try_err!(self.eat(
            RightParentheses,
            |_| ExpectedInformation::new(format!("'{}' to close the condition on a while statement", ')'.fg(Color::Green)),
                 ExpectedCloseParentheses,
                 Some("Cheese requires parentheses around conditions in control structures."),
                 vec![
                     ReportLabel::new(
                         paren_location.clone(),
                         format!("'{}' found here, expected matching '{}'", '('.fg(Color::Green), ')'.fg(Color::Green)),
                         Some(Color::Green),
                     )
                 ],
            )));
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
            While {
                condition,
                body,
                else_statement: els,
            },
        )
    }

    fn parse_loop(&mut self) -> NodePtr {
        let location = self.peek_ref().span.clone();
        self.eat_any();
        let body = self.parse_expression();
        AstNode::new(location.expanded(&body.span),Loop(body))
    }
}