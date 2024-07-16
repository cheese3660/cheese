use std::collections::HashMap;
use ariadne::{Color, Fmt};
use cheese_diagnostics::ErrorCode::{ExpectedClose, ExpectedCloseParentheses, ExpectedColon, ExpectedCommaOrArrow, ExpectedCommaOrClose, ExpectedFieldName, ExpectedMatchBody, ExpectedOpenBrace, ExpectedOpenParentheses, ExpectedSemicolon};
use cheese_diagnostics::locating::FileSpan;
use cheese_diagnostics::ReportLabel;
use cheese_lexer::TokenType;
use cheese_lexer::TokenType::{Arrow, Colon, Comma, Identifier, LeftBrace, LeftParentheses, RightBrace, RightBracket, RightParentheses, Semicolon, ThickArrow};
use crate::ast::{AstNode, AstNodeData, NodePtr};
use crate::ast::AstNodeData::{DestructuringMatchArm, DestructuringMatchArray, DestructuringMatchStructure, DestructuringMatchTuple, EnumLiteral, Match, MatchAll, MatchArm, MatchConstraint, MatchEnumStructure, MatchEnumTuple, MatchRange, MatchValue};
use crate::parser::{ExpectedInformation, NO_NOTE, Parser};
use crate::try_err;

impl Parser {
    pub(super) fn parse_match(&mut self) -> NodePtr {
        let mut arms = vec![];
        let location = self.peek_ref().span.clone();
        self.eat_any();
        let paren_location = self.peek_ref().span.clone();
        try_err!(self.eat(
            LeftParentheses,
            |_| ExpectedInformation::new(format!("'{}' to begin the value in a match statement statement", '('.fg(Color::Green)),
                                         ExpectedOpenParentheses,
                                         Some("Cheese requires parentheses around conditions in control structures."),
                                         vec![
                                             ReportLabel::new(
                                                 location.clone(),
                                                 format!("{} statement begins here, expected '{}' afterwards", "match".fg(Color::Blue), '('.fg(Color::Green)),
                                                 Some(Color::Blue),
                                             )
                                         ],
            )));
        let value = self.parse_expression();

        let close_paren = self.peek_ref().span.clone();
        try_err!(self.eat(
            RightParentheses,
            |_| ExpectedInformation::new(format!("'{}' to close the condition on a match statement", ')'.fg(Color::Green)),
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
        let brace_loc = self.peek_ref().span.clone();
        try_err!(self.eat(
            LeftBrace,
            |_| ExpectedInformation::new(
                format!("'{}' to begin the arms of a match statement", '{'.fg(Color::Green)),
                ExpectedOpenBrace,
                NO_NOTE,
                vec![
                    ReportLabel::new(
                        location.clone(),
                        "match statement begins here",
                        Some(Color::Blue),
                    ),
                    ReportLabel::new(
                        close_paren.clone(),
                        "expected after this token",
                        Some(Color::Green)
                    )
                ]
            )
        ));
        while match self.peek_ref().token_type {
            RightBrace | EndOfFile => false,
            _ => true
        } {
            if self.peek_ref().token_type == Comma {
                self.eat_any();
                continue
            }
            let arm = self.parse_match_arm();
            let requires_comma = if let MatchArm {body, ..} = &arm.data {
                if let AstNodeData::Block(_) = &body.data {
                    false
                } else {
                    true
                }
            } else {
                false
            };
            if requires_comma && match self.peek_ref().token_type {
                RightBrace | Comma => false,
                _ => true
            } {
                _ = self.unexpected(self.peek(),format!("a '{}' or a '{}' following an expression match arm in a match statement",'}'.fg(Color::Green),','.fg(Color::Green)),ExpectedCommaOrClose,NO_NOTE,vec![
                    ReportLabel::new(
                        location.clone(),
                        "match statement begins here",
                        Some(Color::Blue)
                    ),
                    ReportLabel::new(
                        arm.span.clone(),
                        format!("'{}' or '{} expected after this arm",'}'.fg(Color::Green),','.fg(Color::Green)),
                        Some(Color::Red)
                    )
                ])
            }
            arms.push(arm)
        }

        let end_loc = self.peek_ref().span.clone();
        self.eat(RightBrace,|_|ExpectedInformation::new(
            format!("'{}' to close match statement",'}'.fg(Color::Green)),
            ExpectedClose,
            NO_NOTE,
            vec![
                ReportLabel::new(
                    location.clone(),
                    "match statement begins here",
                    Some(Color::Blue),
                ),
                ReportLabel::new(
                    end_loc.clone(),
                    format!("'{}' opens here, expected matching '{}'", '{'.fg(Color::Green), '}'.fg(Color::Green)),
                    Some(Color::Green),
                )
            ]
        )).unwrap_or_else(||AstNode::new(location.expanded(&end_loc),Match { value, arms }))
    }

    fn parse_match_arm(&mut self) -> NodePtr {
        let mut matches = vec![];
        let location = self.peek_ref().span.clone();
        while match self.peek_ref().token_type {
            ThickArrow | Arrow | TokenType::EndOfFile => false,
            _ => true
        } {
            let statement = self.parse_single_match_statement();
            if match self.peek_ref().token_type {
                ThickArrow | Arrow | Comma => false,
                _ => true
            } {
                _ = self.unexpected(self.peek(),format!("a '{}', '{}' or '{}' following a match statement in a match arm",','.fg(Color::Green),"->".fg(Color::Green),"=>".fg(Color::Green)),ExpectedCommaOrArrow,NO_NOTE,vec![
                    ReportLabel::new(
                        location.clone(),
                        "match arm",
                        Some(Color::Blue)
                    ),
                    ReportLabel::new(
                        statement.span.clone(),
                        format!("'{}', '{}' or '{}' expected after this match statement",','.fg(Color::Green),"->".fg(Color::Green),"=>".fg(Color::Green)),
                        Some(Color::Red)
                    )
                ]);
                self.eat_any()
            } else if self.peek_ref().token_type == Comma {
                self.eat_any()
            }
            matches.push(statement);
        }
        // now we actually capture the arrow
        let store = if self.peek_ref().token_type == Arrow {
            self.eat_any();
            Some(self.parse_capture(false))
        } else {
            None
        };

        self.eat(ThickArrow,|_|ExpectedInformation::new(
            format!("a '{}' to begin the body of a match arm","=>".fg(Color::Green)),
            ExpectedMatchBody,
            NO_NOTE,
            vec![
                ReportLabel::new(
                    location.clone(),
                    "match arm begins here",
                    Some(Color::Green)
                )
            ]
        )).unwrap_or_else(
            || {
                let body = self.parse_expression();
                AstNode::new(location.expanded(&body.span),MatchArm {
                    matches,
                    store,
                    body,
                })
            }
        )
    }

    fn parse_single_match_statement(&mut self) -> NodePtr {
        let front = self.peek_ref();
        let location = front.span.clone();
        match front.token_type {
            TokenType::Tuple | TokenType::Object | TokenType::Array => return self.parse_destructuring_match_statement(),
            TokenType::Constrain => {
                self.eat_any();
                let constraint = self.parse_primary(None);
                return AstNode::new(location.expanded(&constraint.span),MatchConstraint(constraint))
            },
            TokenType::Underscore => {
                self.eat_any();
                return AstNode::new(location,MatchAll);
            }
            _ => {}
        }
        let primary = self.parse_primary_base();
        match self.peek_ref().token_type {
            LeftBrace | LeftParentheses | TokenType::LeftBracket => self.parse_enum_destructuring_match_statement(primary),
            TokenType::DoubleDot => {
                self.eat_any();
                let secondary = self.parse_primary(None);
                AstNode::new(location.expanded(&secondary.span),MatchRange { begin: primary, end: secondary })
            },
            _ => AstNode::new(location.expanded(&primary.span),MatchValue(primary))
        }
    }

    fn parse_tuple_or_array_destructuring_match_statement(&mut self, array: bool) -> NodePtr {
        let location = self.peek_ref().span.clone();
        self.eat_any();
        let mut children = vec![];
        let end = if array { RightBracket } else { RightParentheses };
        while match self.peek_ref().token_type {
            TokenType::EndOfFile => false,
            x => x != end
        } {
            if self.peek_ref().token_type == Semicolon {
                self.eat_any();
                continue;
            }
            let arm = self.parse_destructuring_match_arm();
            if match self.peek_ref().token_type {
                Semicolon => false,
                x => x != end
            } {
                children.push(self.unexpected(self.peek(),format!("a '{}' or '{}' after a {} destructure match arm",';'.fg(Color::Green), if array { ']' } else {')'}.fg(Color::Green), if array { "array" } else { "tuple" } ),ExpectedSemicolon,NO_NOTE,vec![
                    ReportLabel::new(
                        location.clone(),
                        "destructuring match statement begins here",
                        Some(Color::Green)
                    ),
                    ReportLabel::new(
                        arm.span.clone(),
                        "expected after this arm",
                        Some(Color::Blue)
                    )
                ]));
            }
            children.push(arm)
        }
        let close_loc = self.peek_ref().span.clone();
        self.eat(end,|_| ExpectedInformation::new(format!("a '{}' to end a {} match statement",if array { ']'} else {')'}.fg(Color::Green), if array {"array"} else {"tuple"}),if array {ExpectedClose} else {ExpectedCloseParentheses}, NO_NOTE,vec![
            ReportLabel::new(
                location.clone(),
                "destructuring match statement begins here",
                Some(Color::Green)
            ),
        ])).unwrap_or_else(|| AstNode::new(location.expanded(&close_loc),if array {
            DestructuringMatchArray(children)
        } else {
            DestructuringMatchTuple(children)
        }))
    }

    fn parse_structure_destructuring_match_statement(&mut self) -> NodePtr {
        let location = self.peek_ref().span.clone();
        self.eat_any();
        let mut children = HashMap::new();
        while match self.peek_ref().token_type {
            TokenType::EndOfFile | RightBrace => false,
            _ => true
        } {
            if self.peek_ref().token_type == Semicolon {
                self.eat_any();
                continue;
            }
            let name = self.peek_ref().value.clone();
            let name_loc = self.peek_ref().span.clone();
            if let Some(_) = self.eat(Identifier,|_| ExpectedInformation::new(
                "a name for a field to match in a structure destructuring match statement",
                ExpectedFieldName,
                NO_NOTE,
                vec![
                    ReportLabel::new(
                        location.clone(),
                        "structure destructuring match statement begins here",
                        Some(Color::Blue)
                    )
                ]
            )) {
                self.eat_any()
            }

            try_err!(self.eat(Colon,|t| ExpectedInformation::new(
                format!("'{}' following the name of a field in a destructuring match statement",':'.fg(Color::Green)),
                ExpectedColon,
                NO_NOTE,
                vec![
                    ReportLabel::new(
                        location.clone(),
                        "structure destructuring match statement begins here",
                        Some(Color::Blue)
                    ),
                    ReportLabel::new(
                        name_loc.clone(),
                        "expected after this identifier here",
                        Some(Color::Blue)
                    )
                ]
            )));

            let arm = self.parse_destructuring_match_arm();

            if match self.peek_ref().token_type {
                Semicolon | RightBrace => false,
                _ => true
            } {
                _ = self.unexpected(self.peek(),format!("a '{}' or '{}' after a destructure match arm",';'.fg(Color::Green), '}'.fg(Color::Green)),ExpectedSemicolon,NO_NOTE,vec![
                    ReportLabel::new(
                        location.clone(),
                        "destructuring match statement begins here",
                        Some(Color::Green)
                    ),
                    ReportLabel::new(
                        arm.span.clone(),
                        "expected after this arm",
                        Some(Color::Blue)
                    )
                ]);
            }
            children.insert(name,arm);
        }
        let close_loc = self.peek_ref().span.clone();
        self.eat(RightBrace,|_| ExpectedInformation::new(format!("a '{}' to end a structure match statement",'}'.fg(Color::Green)),ExpectedClose, NO_NOTE,vec![
            ReportLabel::new(
                location.clone(),
                "destructuring match statement begins here",
                Some(Color::Green)
            ),
        ])).unwrap_or_else(|| AstNode::new(location.expanded(&close_loc),DestructuringMatchStructure(children)))
    }

    #[inline]
    fn parse_destructuring_match_statement(&mut self) -> NodePtr  {
        match self.peek_ref().token_type {
            TokenType::Tuple => self.parse_tuple_or_array_destructuring_match_statement(false),
            TokenType::Array => self.parse_tuple_or_array_destructuring_match_statement(true),
            TokenType::Object => self.parse_structure_destructuring_match_statement(),
            _ => unreachable!()
        }
    }

    fn parse_structure_enum_match_statement(&mut self, ident: &String, location: FileSpan) -> NodePtr {
        self.eat_any();
        let mut children = HashMap::new();
        while match self.peek_ref().token_type {
            TokenType::EndOfFile | RightBrace => false,
            _ => true
        } {
            if self.peek_ref().token_type == Semicolon {
                self.eat_any();
                continue;
            }
            let name = self.peek_ref().value.clone();
            let name_loc = self.peek_ref().span.clone();
            if let Some(_) = self.eat(Identifier,|_| ExpectedInformation::new(
                "a name for a field to match in an enum structure destructuring match statement",
                ExpectedFieldName,
                NO_NOTE,
                vec![
                    ReportLabel::new(
                        location.clone(),
                        "enum structure destructuring match statement begins here",
                        Some(Color::Blue)
                    )
                ]
            )) {
                self.eat_any()
            }

            try_err!(self.eat(Colon,|t| ExpectedInformation::new(
                format!("'{}' following the name of a field in an enum destructuring match statement",':'.fg(Color::Green)),
                ExpectedColon,
                NO_NOTE,
                vec![
                    ReportLabel::new(
                        location.clone(),
                        "enum structure destructuring match statement begins here",
                        Some(Color::Blue)
                    ),
                    ReportLabel::new(
                        name_loc.clone(),
                        "expected after this identifier here",
                        Some(Color::Blue)
                    )
                ]
            )));

            let arm = self.parse_destructuring_match_arm();

            if match self.peek_ref().token_type {
                Semicolon | RightBrace => false,
                _ => true
            } {
                _ = self.unexpected(self.peek(),format!("a '{}' or '{}' after an enum destructure match arm",';'.fg(Color::Green), '}'.fg(Color::Green)),ExpectedSemicolon,NO_NOTE,vec![
                    ReportLabel::new(
                        location.clone(),
                        "enum destructuring match statement begins here",
                        Some(Color::Green)
                    ),
                    ReportLabel::new(
                        arm.span.clone(),
                        "expected after this arm",
                        Some(Color::Blue)
                    )
                ]);
            }
            children.insert(name,arm);
        }
        let close_loc = self.peek_ref().span.clone();
        self.eat(RightBrace,|_| ExpectedInformation::new(format!("a '{}' to end an enum structure match statement",'}'.fg(Color::Green)),ExpectedClose, NO_NOTE,vec![
            ReportLabel::new(
                location.clone(),
                "enum destructuring match statement begins here",
                Some(Color::Green)
            ),
        ])).unwrap_or_else(|| AstNode::new(location.expanded(&close_loc),MatchEnumStructure{
            enum_identifier: ident.clone(),
            children
        }))
    }

    fn parse_tuple_enum_match_statement(&mut self, ident: &String, location: FileSpan) -> NodePtr {
        self.eat_any();
        let mut children = vec![];
        let end = RightParentheses;
        while match self.peek_ref().token_type {
            TokenType::EndOfFile => false,
            x => x != end
        } {
            if self.peek_ref().token_type == Semicolon {
                self.eat_any();
                continue;
            }
            let arm = self.parse_destructuring_match_arm();
            if match self.peek_ref().token_type {
                Semicolon => false,
                x => x != end
            } {
                children.push(self.unexpected(self.peek(),format!("a '{}' or '{}' after a enum tuple destructure match arm",';'.fg(Color::Green), ')'.fg(Color::Green)),ExpectedSemicolon,NO_NOTE,vec![
                    ReportLabel::new(
                        location.clone(),
                        "enum destructuring match statement begins here",
                        Some(Color::Green)
                    ),
                    ReportLabel::new(
                        arm.span.clone(),
                        "expected after this arm",
                        Some(Color::Blue)
                    )
                ]));
            }
            children.push(arm)
        }
        let close_loc = self.peek_ref().span.clone();
        self.eat(end,|_| ExpectedInformation::new(format!("a '{}' to end a enum tuple match statement",')'.fg(Color::Green)),ExpectedCloseParentheses, NO_NOTE,vec![
            ReportLabel::new(
                location.clone(),
                "enum destructuring match statement begins here",
                Some(Color::Green)
            ),
        ])).unwrap_or_else(|| AstNode::new(location.expanded(&close_loc), MatchEnumTuple {
            enum_identifier: ident.clone(),
            children,
        }))
    }
    fn parse_enum_destructuring_match_statement(&mut self, primary: NodePtr) -> NodePtr {
        let location = primary.span.clone();
        match &primary.data {
            EnumLiteral(name) => {
                if self.peek_ref().token_type == LeftBrace {
                    self.parse_structure_enum_match_statement(name, location)
                } else if self.peek_ref().token_type == LeftParentheses {
                    self.parse_tuple_enum_match_statement(name,location)
                } else {
                    self.expand_primary(primary, location)
                }
            },
            _ => {
                self.expand_primary(primary, location)
            }
        }
    }

    fn expand_primary(&mut self, primary: NodePtr, location: FileSpan) -> NodePtr {
        let primary = self.parse_primary(Some(primary));
        if self.peek_ref().token_type == TokenType::DoubleDot {
            self.eat_any();
            let secondary = self.parse_primary(None);
            AstNode::new(location.expanded(&secondary.span), MatchRange { begin: primary, end: secondary })
        } else {
            AstNode::new(location, MatchValue(primary))
        }
    }

    fn parse_destructuring_match_arm(&mut self) -> NodePtr {
        let mut matches = vec![];
        let location = self.peek_ref().span.clone();
        while match self.peek_ref().token_type {
            Semicolon | Arrow | TokenType::EndOfFile | RightBrace | RightBracket | RightParentheses => false,
            _ => true
        } {
            let statement = self.parse_single_match_statement();
            if match self.peek_ref().token_type {
                Semicolon | Arrow | Comma | RightBrace | RightBracket | RightParentheses => false,
                _ => true
            } {
                _ = self.unexpected(self.peek(),format!("a '{}', '{}', '{}', '{}', '{}', or '{}' following a match statement in a destructuring match arm",','.fg(Color::Green),"->".fg(Color::Green),";".fg(Color::Green),")".fg(Color::Green),"]".fg(Color::Green),"}".fg(Color::Green)),ExpectedCommaOrArrow,NO_NOTE,vec![
                    ReportLabel::new(
                        location.clone(),
                        "match arm",
                        Some(Color::Blue)
                    ),
                    ReportLabel::new(
                        statement.span.clone(),
                        format!("'{}', '{}' or '{}' expected after this match statement",','.fg(Color::Green),"->".fg(Color::Green),";".fg(Color::Green)),
                        Some(Color::Red)
                    )
                ]);
                self.eat_any()
            } else if self.peek_ref().token_type == Comma {
                self.eat_any()
            }
            matches.push(statement);
        }
        // now we actually capture the arrow
        let store = if self.peek_ref().token_type == Arrow {
            self.eat_any();
            Some(self.parse_capture(false))
        } else {
            None
        };

        AstNode::new(location.expanded(match &store {
            None => match matches.last() {
                None => &location,
                Some(last) => &last.span
            },
            Some(capture) => &capture.span
        }), DestructuringMatchArm {
            matches,
            store
        })
    }
}