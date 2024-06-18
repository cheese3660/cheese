use ariadne::{Color, Fmt, ColorGenerator};
use std::{collections::HashMap};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use ariadne::IndexType::Char;
use lazy_static::lazy_static;
use cheese_diagnostics;
use cheese_diagnostics::{advice, error, ErrorCode, exiting_error, ReportLabel};
use cheese_diagnostics::ErrorCode::{ExpectedSelf, UnterminatedBlockComment};
use cheese_diagnostics::locating::{Coordinate, File, FileSpan};
use crate::TokenType::{AddAssign, Ampersand, Array, Arrow, Assign, Block, Cast, CharacterLiteral, Colon, Comma, ConstantArray, ConstantPointer, ConstantSlice, ConstSelfType, Dash, DivideAssign, Dot, DoubleColon, DoubleDot, DynamicCast, EndOfFile, EqualTo, Exclamation, GreaterThan, GreaterThanEqual, Hash, LeftBrace, LeftBracket, LeftParentheses, LeftShift, LeftShiftAssign, LessThan, LessThanEqual, ModuloAssign, MultiplyAssign, NotEqualTo, Object, Percent, Pipe, Plus, Question, Redefine, ReversedArrow, RightBrace, RightBracket, RightParentheses, RightShift, RightShiftAssign, Semicolon, Slash, Star, StringLiteral, SubtractAssign, ThickArrow, TripleDot, Tuple};

// Important types


#[derive(Clone, Debug, PartialEq, Copy)]
pub enum TokenType {
    // Semantic tokens
    Arrow, // ->
    Exclamation, // !
    ThickArrow, // =>
    ReversedArrow, // <-
    Colon, // :
    DoubleColon, // ::
    Comma, // ,
    Semicolon, // ;
    Dot, // .
    DoubleDot, // ..
    TripleDot, // ...

    Hash, // # Used for finding out the length of an array or slice
    Question, // ? Used for denoting an unknown sized array of items
    ConstantPointer, // *~
    ConstantArray, // ]~
    ConstantSlice, // >~
    Cast, // @

    // Operator Tokens
    Star, // * Used for pointers, dereferencing, and multiplication
    Percent, // % Modulo
    Slash, // / Divide
    Plus, // + Add
    Dash, // - Subtract
    Ampersand, // & Reference
    LeftShift, // <<
    RightShift, // >>
    GreaterThan, // >
    LessThan, // <
    EqualTo, // ==
    NotEqualTo, // !=
    GreaterThanEqual, // >=
    LessThanEqual, // <=
    And, // and
    Or, // or
    Xor, // xor
    AndAssign, // and=
    OrAssign, // or=
    XorAssign, // xor=
    Not, // not
    MultiplyAssign, // *=
    ModuloAssign, // %=
    DivideAssign, // /=
    AddAssign, // +=
    SubtractAssign, // -=
    LeftShiftAssign, // <<=
    RightShiftAssign, // >>=
    Tuple, // .(
    Object, // .{
    Array, // .[
    Block, // :( name )
    BlockYield, // yield( name ) ...
    Exponentiate, // ^
    ExponentiateAssign, // ^=
    Redefine, // :=
    DynamicCast, // @*
    Assign, // =
    Dereference, // $


    // Structural tokens
    LeftBrace, // {
    RightBrace, // }
    LeftBracket, // [
    RightBracket, // ]
    LeftParentheses, // (
    RightParentheses, // )



    // Textual tokens
    Public, // public
    Private, // private
    Mutable, // mut
    NoReturn, // noreturn
    Enum, // enum
    Union, // union
    Struct, // struct
    Extern, // extern
    Let, // let
    Def, // def
    Fn, // fn
    Float32, // f32
    Float64, // f64
    Complex32, // c32
    Complex64, // c64
    Match, // match
    If, // if
    Else, // else
    While, // while
    For, // for
    Import, // import
    Type, // type
    None, // none
    Void, // void
    Prototype, // prototype
    Inline, // inline
    Entry, // entry
    CompileTime, // comptime
    Assembly, // asm (depends on the target)
    CompileTimeString, // cstr (compile time known string type, can only be used in template parameters
    Template, // template
    CompileTimeFloat, // comptime_float
    CompileTimeComplex, // comptime_complex
    CompileTimeInt, // comptime_int
    UnsignedSize, // usize (size_t in the C target)
    SignedSize, // isize (ptrdiff_t in the C target)

    // Dynamic types
    SignedIntegerType,
    UnsignedIntegerType,
    FloatingLiteral,
    ImaginaryLiteral,
    DecimalLiteral,
    HexadecimalLiteral,
    OctalLiteral,
    BinaryLiteral,
    Identifier,
    BuiltinReference,
    StringLiteral,
    CharacterLiteral,
    SingleLineComment,
    BlockComment,
    Underscore, // _
    Yield, // yield
    Break, // break (same as yield none)
    Continue, // continue
    Return, // return
    Bool, // bool
    True, // true
    False, // false
    Then, // Used for inline if statements, without brackets, with brackets a "yield" is expected inside
    Pipe, // |
    Generator, // generator
    Is, // is
    Opaque, // opaque
    Export, // export
    Impl, // impl
    Interface, // interface
    Dynamic, // dynamic
    With, // with
    Constrain, // constrain
    Loop, // loop
    SelfType, // self
    ConstSelfType, // self~
    TypeSelf, // Self
    Operator, // operator
    Concept, // concept
    // Special
    EndOfFile,
    // Lexer error token
    Error,
}

lazy_static! {
    static ref BUILTIN_MACROS: Vec<&'static str> = {
        vec!["try","Err","Ok","Result"]
    };

    static ref RESERVED_TOKENS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("public",TokenType::Public);
        m.insert("private",TokenType::Private);
        m.insert("mut", TokenType::Mutable);
        m.insert("noreturn", TokenType::NoReturn);
        m.insert("enum", TokenType::Enum);
        m.insert("union", TokenType::Union);
        m.insert("struct", TokenType::Struct);
        m.insert("extern", TokenType::Extern);
        m.insert("let", TokenType::Let);
        m.insert("def", TokenType::Def);
        m.insert("fn", TokenType::Fn);
        m.insert("f32", TokenType::Float32);
        m.insert("f64", TokenType::Float64);
        m.insert("c32", TokenType::Complex32);
        m.insert("c64", TokenType::Complex64);
        m.insert("match", TokenType::Match);
        m.insert("if", TokenType::If);
        m.insert("else", TokenType::Else);
        m.insert("while", TokenType::While);
        m.insert("for", TokenType::For);
        m.insert("type", TokenType::Type);
        m.insert("none", TokenType::None);
        m.insert("void", TokenType::Void);
        m.insert("import", TokenType::Import);
        m.insert("prototype", TokenType::Prototype);
        m.insert("inline", TokenType::Inline);
        m.insert("entry", TokenType::Entry);
        m.insert("comptime", TokenType::CompileTime);
        m.insert("comptime_string", TokenType::CompileTimeString);
        m.insert("asm", TokenType::Assembly);
        m.insert("comptime_float", TokenType::CompileTimeFloat);
        m.insert("comptime_complex", TokenType::CompileTimeComplex);
        m.insert("comptime_int", TokenType::CompileTimeInt);
        m.insert("usize", TokenType::UnsignedSize);
        m.insert("isize", TokenType::SignedSize);
        m.insert("continue", TokenType::Continue);
        m.insert("break", TokenType::Break);
        m.insert("then", TokenType::Then);
        m.insert("with", TokenType::With);
        m.insert("_", TokenType::Underscore);
        m.insert("and", TokenType::And);
        m.insert("and=", TokenType::AndAssign);
        m.insert("xor", TokenType::Xor);
        m.insert("xor=", TokenType::XorAssign);
        m.insert("or", TokenType::Or);
        m.insert("or=", TokenType::OrAssign);
        m.insert("not", TokenType::Not);
        m.insert("bool", TokenType::Bool);
        m.insert("true", TokenType::True);
        m.insert("false", TokenType::False);
        m.insert("generator", TokenType::Generator);
        m.insert("is", TokenType::Is);
        m.insert("opaque", TokenType::Opaque);
        m.insert("export", TokenType::Export);
        m.insert("impl", TokenType::Impl);
        m.insert("interface", TokenType::Interface);
        m.insert("dynamic", TokenType::Dynamic);
        m.insert("constrain", TokenType::Constrain);
        m.insert("loop", TokenType::Loop);
        m.insert("self", TokenType::SelfType);
        m.insert("Self", TokenType::TypeSelf);
        m.insert("operator", TokenType::Operator);
        m.insert("return", TokenType::Return);
        m.insert("yield", TokenType::Yield);
        m.insert("yield(", TokenType::BlockYield);
        m.insert("template", TokenType::Template);
        m.insert("concept", TokenType::Concept);
        return m;
    };
}

#[derive(Clone, Debug)]
pub struct Token {
    /// The type of this token
    pub token_type: TokenType,
    /// The slice of this token
    pub value: String,
    pub span: FileSpan,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} .. {}): {:?} - {}", self.span.begin.pos, self.span.end.pos,self.token_type,self.value)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct LexerConfiguration {
    error_invalid: bool,
    output_comments: bool,
    warn_comments: bool,
}

impl Default for LexerConfiguration {
    fn default() -> Self {
        LexerConfiguration {
            error_invalid: true,
            output_comments: false,
            warn_comments: true,
        }
    }
}

pub struct Lexer {
    file: Rc<File>,
    buffer_position: usize,
    buffer_position_bytes: usize,
    peek_value: Option<char>,
    configuration: LexerConfiguration,
}

impl Lexer {
    pub fn create(file: Rc<File>, configuration: LexerConfiguration) -> Lexer {
        let mut result: Lexer = Lexer {
            file,
            buffer_position: 0,
            buffer_position_bytes: 0,
            peek_value: None,
            configuration,
        };
        result.peek_value = result.file.data.chars().next();
        result.buffer_position_bytes = match result.peek_value {
            Some(c) => c.len_utf8(),
            None => 0
        };
        result
    }

    fn peek(&self) -> Option<char> {
        self.peek_value
    }

    fn advance(&mut self) {
        match self.peek() {
            None => return,
            _ => {}
        }
        self.buffer_position += 1;

        self.peek_value = self.file.data[self.buffer_position_bytes..].chars().next();
        self.buffer_position_bytes += match self.peek_value {
            Some(c) => c.len_utf8(),
            None => 0
        };
    }

    fn location(&self) -> Coordinate {
        Coordinate {
            file: Rc::clone(&self.file),
            pos: self.buffer_position,
        }
    }

    fn eof(&self) -> bool {
        self.peek_value == None
    }

    fn span_value(&self, begin: usize, end: usize) -> String {
        self.file.data.chars().skip(begin).take(end - begin).collect()
    }

    fn span_from(&self, begin: usize) -> String {
        self.span_value(begin, self.location().pos)
    }

    fn file_span(&self, begin: Coordinate) -> FileSpan {
        FileSpan {
            begin,
            end: self.location()
        }
    }

    fn skip_multiline_comment(&mut self, start_location: Coordinate) -> Token {
        loop {
            if self.eof() {
                if self.configuration.error_invalid {
                    exiting_error(start_location.clone(), UnterminatedBlockComment, "unterminated block comment".to_owned(), None, vec![ReportLabel::new(
                        FileSpan {
                            begin: start_location.clone(),
                            end: self.location()
                        },
                        "unterminated comment".to_owned(),
                        Some(Color::Red),
                    )]);
                } else {
                    return Token {
                        token_type: TokenType::Error,
                        span: FileSpan {
                            begin: start_location.clone(),
                            end: self.location()
                        },
                        value: self.span_from(start_location.pos),
                    };
                }
            }
            let current = self.peek();
            let mut depth = 1;
            match current {
                None => unreachable!(),
                Some('*') => {
                    self.advance();
                    let lookahead = self.peek();
                    if lookahead == Some('/') {
                        depth -= 1;
                        self.advance();
                        if depth <= 0 {
                            break;
                        }
                    }
                }
                Some('/') => {
                    self.advance();
                    let lookahead = self.peek();
                    if lookahead == Some('*') {
                        self.advance();
                        depth += 1;
                    }
                }
                Some(_) => {
                    self.advance()
                }
            }
        }
        let comment = self.span_from(start_location.pos);
        if self.configuration.warn_comments {
            check_comment(start_location.clone(), &comment);
        }
        Token {
            token_type: TokenType::BlockComment,
            value: comment,
            span: FileSpan {
                begin: start_location.clone(),
                end: self.location(),
            },
        }
    }

    fn skip_comment(&mut self, start_location: Coordinate) -> Token {
        while !self.eof() {
            let current = self.peek_value;
            self.advance();
            match current {
                None => break,
                Some('\n') => break,
                _ => ()
            };
        }
        let end_pos = self.location();
        let comment = self.span_value(start_location.pos, end_pos.pos);
        if self.configuration.warn_comments {
            check_comment(start_location.clone(), &comment);
        }

        Token {
            token_type: TokenType::SingleLineComment,
            span: FileSpan {
                begin: start_location,
                end: end_pos,
            },
            value: comment,
        }
    }

    fn string(&mut self) -> Token {
        let start = self.location();
        let mut in_escape = false;
        self.advance();
        loop {
            let current = self.peek();
            if current.is_none() || current == Some('\n') {
                if self.configuration.error_invalid {
                    error(start.clone(), ErrorCode::UnterminatedStringLiteral, "unterminated string literal".to_string(), None, vec![
                        ReportLabel::new(
                            FileSpan {
                                begin: start.clone(),
                                end: self.location()
                            },
                            "unterminated string literal".to_owned(),
                            Some(Color::Red),
                        )
                    ])
                }
                return Token {
                    token_type: TokenType::Error,
                    span: FileSpan {
                        begin: start.clone(),
                        end: self.location()
                    },
                    value: self.span_from(start.pos),
                };
            }
            self.advance();
            match current {
                None => unreachable!(),
                Some('"') => {
                    if in_escape {
                        in_escape = false;
                    } else {
                        break;
                    }
                }
                Some('\\') => {
                    in_escape = !in_escape;
                }
                Some(_) => {
                    in_escape = false
                }
            }
        }
        Token {
            token_type: StringLiteral,
            span: FileSpan {
                begin: start.clone(),
                end: self.location(),
            },
            value: self.span_from(start.pos),
        }
    }

    fn character(&mut self) -> Token {
        let start = self.location();
        let mut in_escape = false;
        self.advance();
        loop {
            let current = self.peek();
            if current.is_none() || current == Some('\n') {
                if self.configuration.error_invalid {
                    error(start.clone(), ErrorCode::UnterminatedCharacterLiteral, "unterminated character literal".to_string(), None, vec![
                        ReportLabel::new(
                            FileSpan {
                                begin: start.clone(),
                                end: self.location()
                            },
                            "unterminated character literal".to_owned(),
                            Some(Color::Red),
                        )
                    ])
                }
                return Token {
                    token_type: TokenType::Error,
                    span: FileSpan {
                        begin: start.clone(),
                        end: self.location()
                    },
                    value: self.span_from(start.pos),
                };
            }
            self.advance();
            match current {
                None => unreachable!(),
                Some('\'') => {
                    if in_escape {
                        in_escape = false;
                    } else {
                        break;
                    }
                }
                Some('\\') => {
                    in_escape = !in_escape;
                }
                Some(_) => {
                    in_escape = false
                }
            }
        }
        Token {
            token_type: CharacterLiteral,
            span: FileSpan {
                begin: start.clone(),
                end: self.location(),
            },
            value: self.span_from(start.pos),
        }
    }

    fn binary(&mut self) {
        loop {
            match self.peek() {
                None => break,
                Some(v) => if valid_binary(v) {
                    self.advance()
                } else {
                    break;
                }
            }
        }
    }

    fn octal(&mut self) {
        loop {
            match self.peek() {
                None => break,
                Some(v) => if valid_octal(v) {
                    self.advance()
                } else {
                    break;
                }
            }
        }
    }

    fn hexadecimal(&mut self) {
        loop {
            match self.peek() {
                None => break,
                Some(v) => if valid_hex(v) {
                    self.advance()
                } else {
                    break;
                }
            }
        }
    }

    fn number(&mut self) -> Token {
        let start_location = self.location();
        let mut float = false;
        let mut imaginary = false;
        if self.peek() == Some('0') {
            self.advance();
            let next = self.peek();
            match next {
                Some('x') | Some('X') => {
                    self.advance();
                    self.hexadecimal();
                    return Token {
                        token_type: TokenType::HexadecimalLiteral,
                        span: FileSpan {
                            begin: start_location.clone(),
                            end: self.location(),
                        },
                        value: self.span_from(start_location.pos),
                    };
                }
                Some('o') | Some('O') => {
                    self.advance();
                    self.octal();
                    return Token {
                        token_type: TokenType::OctalLiteral,
                        span: FileSpan {
                            begin: start_location.clone(),
                            end: self.location(),
                        },
                        value: self.span_from(start_location.pos),
                    };
                }
                Some('b') | Some('B') => {
                    self.advance();
                    self.binary();
                    return Token {
                        token_type: TokenType::BinaryLiteral,
                        span: FileSpan {
                            begin: start_location.clone(),
                            end: self.location(),
                        },
                        value: self.span_from(start_location.pos),
                    };
                }
                _ => {}
            }
        }
        while match self.peek() {
            Some(c) => valid_decimal_middle(c),
            None => false
        } {
            self.advance();
        }

        if self.peek() == Some('.') {
            float = true;
            self.advance();
            while match self.peek() {
                Some(c) => valid_decimal_middle(c),
                None => false
            } {
                self.advance();
            }
        }

        if match self.peek() {
            Some('e') | Some('E') => true,
            _ => false
        } {
            float = true;
            self.advance();
            if match self.peek() {
                Some('-') | Some('+') => true,
                _ => false
            } {
                self.advance();
            }
            while match self.peek() {
                Some(c) => valid_decimal_middle(c),
                None => false
            } {
                self.advance();
            }
        }

        if self.peek() == Some('I') {
            imaginary = true;
            self.advance();
        }

        Token {
            token_type: if imaginary { TokenType::ImaginaryLiteral } else if float { TokenType::FloatingLiteral } else { TokenType::DecimalLiteral },
            span: FileSpan {
                begin: start_location.clone(),
                end: self.location(),
            },
            value: self.span_from(start_location.pos),
        }
    }

    fn identifier(&mut self) -> Token {
        let start_location = self.location();
        while match self.peek() {
            None => false,
            Some(c) => valid_identifier_middle(c)
        } {
            self.advance();
        }
        match self.peek() {
            Some('(') | Some('=') => {
                let value = self.span_value(start_location.pos, self.buffer_position + 1);
                let kw = RESERVED_TOKENS.get(&value.as_str());
                if let Some(v) = kw {
                    self.advance();
                    return Token {
                        token_type: v.clone(),
                        span: FileSpan {
                            begin: start_location,
                            end: self.location(),
                        },
                        value,
                    };
                }
            }
            _ => {}
        }
        let value = self.span_from(start_location.pos);
        let opt = RESERVED_TOKENS.get(&value.as_str());
        let opt2 = BUILTIN_MACROS.contains(&value.as_str());
        Token {
            span: FileSpan {
                begin: start_location,
                end: self.location(),
            },
            token_type: match opt {
                Some(ty) => ty.clone(),
                None => if opt2 {
                    TokenType::BuiltinReference
                } else if valid_integer_type(value.as_str()) {
                    if value.starts_with("u") {
                        TokenType::UnsignedIntegerType
                    } else {
                        TokenType::SignedIntegerType
                    }
                } else {
                    TokenType::Identifier
                }
            },
            value,
        }
    }

    fn builtin(&mut self) -> Token {
        let start_location = self.location();
        self.advance();
        let mut advanced = false;
        while match self.peek() {
            None => false,
            Some(c) => valid_identifier_middle(c)
        } {
            advanced = true;
            self.advance();
        }
        Token {
            token_type: if advanced {
                TokenType::BuiltinReference
            } else {
                TokenType::Dereference
            },
            value: self.span_from(start_location.pos),
            span: FileSpan {
                begin: start_location,
                end: self.location(),
            },
        }
    }


    fn make_token(&self, ty: TokenType, start: Coordinate) -> Token {
        Token {
            token_type: ty,
            value: self.span_from(start.pos),
            span: FileSpan {
                begin: start,
                end: self.location()
            }
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while !self.eof() {
            // println!("Got character {current:?}");
            let start_location = self.location();
            match self.peek() {
                Some('=') => {
                    self.advance();
                    match self.peek() {
                        Some('=') => {
                            self.advance();
                            tokens.push(self.make_token(EqualTo, start_location));
                        },
                        Some('>') => {
                            self.advance();
                            tokens.push(self.make_token(ThickArrow, start_location));
                        }
                        _ => tokens.push(self.make_token(Assign, start_location))
                    }
                },
                Some('/') => {
                    self.advance();
                    match self.peek() {
                        Some('=') => {
                            self.advance();
                            tokens.push(self.make_token(DivideAssign, start_location));
                        },
                        Some('*') => {
                            self.advance();
                            let comment = self.skip_multiline_comment(start_location);
                            if self.configuration.output_comments {
                                tokens.push(comment)
                            }
                        },
                        Some('/') => {
                            self.advance();
                            let comment = self.skip_comment(start_location);
                            if self.configuration.output_comments {
                                tokens.push(comment);
                            }
                        }
                        _ => tokens.push(self.make_token(Slash, start_location))
                    }
                },
                Some('-') => {
                    self.advance();
                    match self.peek() {
                        Some('=') => {
                            self.advance();
                            tokens.push(self.make_token(SubtractAssign, start_location));
                        },
                        Some('>') => {
                            self.advance();
                            tokens.push(self.make_token(Arrow, start_location));
                        },
                        _ => tokens.push(self.make_token(Dash, start_location))
                    }
                },
                Some('\'') => tokens.push(self.character()),
                Some('\"') => tokens.push(self.string()),
                Some('.') => {
                    self.advance();
                    match self.peek() {
                        Some('.') => {
                            self.advance();
                            if let Some('.') = self.peek() {
                                self.advance();
                                tokens.push(self.make_token(TripleDot,start_location));
                            } else {
                                tokens.push(self.make_token(DoubleDot,start_location));
                            }
                        },
                        Some('[') => {
                            self.advance();
                            tokens.push(self.make_token(Array, start_location));
                        },
                        Some('(') => {
                            self.advance();
                            tokens.push(self.make_token(Tuple, start_location));
                        },
                        Some('{') => {
                            self.advance();
                            tokens.push(self.make_token(Object, start_location));
                        },
                        _ => tokens.push(self.make_token(Dot, start_location)),
                    }
                },
                Some(':') => {
                    self.advance();
                    match self.peek() {
                        Some('=') => {
                            self.advance();
                            tokens.push(self.make_token(Redefine, start_location));
                        },
                        Some('{') => {
                            self.advance();
                            tokens.push(self.make_token(Block, start_location));
                        },
                        Some(':') => {
                            self.advance();
                            tokens.push(self.make_token(DoubleColon, start_location));
                        },
                        _ => tokens.push(self.make_token(Colon, start_location))
                    }
                }
                Some(',') => {
                    self.advance();
                    tokens.push(self.make_token(Comma, start_location));
                },
                Some(';') => {
                    self.advance();
                    tokens.push(self.make_token(Semicolon, start_location));
                },
                Some('*') => {
                    self.advance();
                    match self.peek() {
                        Some('=') => {
                            self.advance();
                            tokens.push(self.make_token(MultiplyAssign, start_location));
                        },
                        Some('~') => {
                            self.advance();
                            tokens.push(self.make_token(ConstantPointer, start_location));
                        }
                        _ => tokens.push(self.make_token(Star, start_location))
                    }
                },
                Some('&') => {
                    self.advance();
                    tokens.push(self.make_token(Ampersand, start_location));
                },
                Some('@') => {
                    self.advance();
                    if let Some('*') = self.peek() {
                        self.advance();
                        tokens.push(self.make_token(DynamicCast, start_location));
                    } else {
                        tokens.push(self.make_token(Cast, start_location));
                    }
                },
                Some('{') => {
                    self.advance();
                    tokens.push(self.make_token(LeftBrace, start_location));
                },
                Some('}') => {
                    self.advance();
                    tokens.push(self.make_token(RightBrace, start_location));
                },
                Some('(') => {
                    self.advance();
                    tokens.push(self.make_token(LeftParentheses, start_location));
                },
                Some(')') => {
                    self.advance();
                    tokens.push(self.make_token(RightParentheses, start_location));
                },
                Some('[') => {
                    self.advance();
                    tokens.push(self.make_token(LeftBracket, start_location));
                },
                Some(']') => {
                    self.advance();
                    if let Some('~') = self.peek() {
                        self.advance();
                        tokens.push(self.make_token(ConstantArray, start_location));
                    } else {
                        tokens.push(self.make_token(RightBracket, start_location));
                    }
                },
                Some('#') => {
                    self.advance();
                    tokens.push(self.make_token(Hash, start_location));
                },
                Some('%') => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        tokens.push(self.make_token(ModuloAssign, start_location));
                    } else {
                        tokens.push(self.make_token(Percent, start_location))
                    }
                },
                Some('<') => {
                    self.advance();
                    match self.peek() {
                        Some('=') => {
                            self.advance();
                            tokens.push(self.make_token(LessThanEqual, start_location));
                        },
                        Some('<') => {
                            self.advance();
                            if let Some('=') = self.peek() {
                                tokens.push(self.make_token(LeftShiftAssign, start_location));
                            } else {
                                tokens.push(self.make_token(LeftShift, start_location));
                            }
                        },
                        Some('-') => {
                            self.advance();
                            tokens.push(self.make_token(ReversedArrow, start_location));
                        },
                        _ => tokens.push(self.make_token(LessThan, start_location))
                    }
                },
                Some('>') => {
                    self.advance();
                    match self.peek() {
                        Some('=') => {
                            self.advance();
                            tokens.push(self.make_token(GreaterThanEqual, start_location));
                        },
                        Some('>') => {
                            self.advance();
                            if let Some('=') = self.peek() {
                                tokens.push(self.make_token(RightShiftAssign, start_location));
                            } else {
                                tokens.push(self.make_token(RightShift, start_location));
                            }
                        },
                        Some('~') => {
                            self.advance();
                            tokens.push(self.make_token(ConstantSlice, start_location));
                        },
                        _ => tokens.push(self.make_token(GreaterThan, start_location))
                    }
                },
                Some('!') => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        tokens.push(self.make_token(NotEqualTo, start_location));
                    } else {
                        tokens.push(self.make_token(Exclamation, start_location));
                    }
                },
                Some('+') => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.advance();
                        tokens.push(self.make_token(AddAssign, start_location));
                    } else {
                        tokens.push(self.make_token(Plus, start_location));
                    }
                },
                Some('$') => {
                    tokens.push(self.builtin());
                },
                Some('?') => {
                    self.advance();
                    tokens.push(self.make_token(Question, start_location));
                },
                Some('|') => {
                    self.advance();
                    tokens.push(self.make_token(Pipe, start_location));
                },
                Some('~') => {
                    self.advance();
                    let mut ident = self.identifier();
                    if ident.token_type == TokenType::SelfType {
                        ident.span = self.file_span(start_location);
                        ident.token_type = ConstSelfType;
                        ident.value = "~self".to_string();
                        tokens.push(ident);
                    } else {
                        // Lets do this
                        let mut note: Option<String> = None;
                        let mut cg = ColorGenerator::new();
                        let a = cg.next();
                        let mut labels = vec![ReportLabel::new(self.file_span(start_location.clone()),format!("expected '{}' after the '{}'","self".fg(a),'~'.fg(a)),Some(a))];
                        if let Some(t) = tokens.last() {
                            let c = cg.next();
                            if t.token_type == GreaterThan {
                                labels.push(ReportLabel::new(t.span.clone(),format!("'{}' found before '~', are you trying to make a constant slice?",t.value.clone().fg(c)),Some(c)));
                                note = Some("if you are trying to make a constant slice '>~' there cannot be space between the '>' and '~'".to_string());
                            } else if t.token_type == Star {
                                labels.push(ReportLabel::new(t.span.clone(),format!("'{}' found before '~', are you trying to make a constant pointer?",t.value.clone().fg(c)),Some(c)));
                                note = Some("if you are trying to make a constant pointer '*~' there cannot be space between the '*' and '~'".to_string());
                            }
                        }
                        error(start_location,ExpectedSelf,"expected self".to_string(),note,labels);
                    }
                }
                Some(x) => {
                    if x.is_whitespace() {
                        self.advance()
                    } else if valid_identifier_beginning(x) {
                        tokens.push(self.identifier());
                    } else if valid_decimal_beginning(x) {
                        tokens.push(self.number());
                    } else if self.configuration.error_invalid {
                        self.advance();
                        error(start_location.clone(), ErrorCode::UnexpectedCharacter, format!("unexpected character '{}'", x.fg(Color::Red)), None, vec![
                            ReportLabel::new(
                                FileSpan {
                                    begin: start_location,
                                    end: self.location(),
                                },
                                "offending character".to_string(),
                                Some(Color::Red),
                            )
                        ]);
                    }
                }
                None => unreachable!()
            }
        }
        tokens.push(Token{
            token_type: EndOfFile,
            value: "".to_string(),
            span: self.file_span(self.location())
        });
        tokens
    }
}

fn valid_binary(c: char) -> bool {
    c.is_digit(2) || c == '_'
}

fn valid_octal(c: char) -> bool {
    c.is_digit(8) || c == '_'
}

fn valid_hex(c: char) -> bool {
    c.is_digit(16) || c == '_'
}

fn valid_decimal_beginning(c: char) -> bool {
    c.is_digit(10)
}

fn valid_decimal_middle(c: char) -> bool {
    c.is_digit(10) || c == '_'
}

fn valid_identifier_beginning(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn valid_identifier_middle(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn valid_integer_type(ty: &str) -> bool {
    let mut iter = ty.chars();
    let first = iter.next();
    let mut first_char = true;
    match first {
        Some('u') | Some('i') => loop {
            let next = iter.next();
            match next {
                Some(x) => if !x.is_digit(10) {
                    break false;
                }
                None => break if first_char {
                    false
                } else {
                    true
                }
            }
            first_char = false;
        },
        _ => false,
    }
}

fn check_comment(comment_location: Coordinate, comment: &str) {
    let file = comment_location.clone().file;
    if let Some(location) = comment.find("TODO") {
        let begin = comment_location.pos + location;
        let end = begin + 4;
        let warn_span = FileSpan {
            begin: Coordinate {
                file: Rc::clone(&file),
                pos: begin
            },
            end: Coordinate {
                file: Rc::clone(&file),
                pos: end
            }
        };
        advice(
            comment_location.clone(),
            ErrorCode::TodoFoundInComment,
            format!("'{}' found inside a comment, maybe you should look into this", "TODO".fg(Color::Yellow)),
            Some(format!(
                "Cheese looks for keywords like '{}' in comments, because these keywords generally signify that code is incomplete or could be buggy, the compiler option --no-comment-advice disables this.",
                "TODO".fg(Color::Yellow))),
            vec![
                ReportLabel::new(
                    warn_span,
                    format!("'{}' found here", "TODO".fg(Color::Yellow)),
                    Some(Color::Yellow),
                )
            ]);
    }
    if let Some(location) = comment.find("FIXME") {
        let begin = comment_location.pos + location;
        let end = begin + 5;
        let warn_span = FileSpan {
            begin: Coordinate {
                file: Rc::clone(&file),
                pos: begin
            },
            end: Coordinate {
                file: Rc::clone(&file),
                pos: end
            }
        };
        advice(
            comment_location.clone(),
            ErrorCode::FixmeFoundInComment,
            format!("'{}' found inside a comment, maybe you should look into this", "FIXME".fg(Color::Yellow)),
            Some(format!(
                "Cheese looks for keywords like '{}' in comments, because these keywords generally signify that code is incomplete or could be buggy, the compiler option --no-comment-advice disables this.",
                "FIXME".fg(Color::Yellow))),
            vec![
                ReportLabel::new(
                    warn_span,
                    format!("'{}' found here", "FIXME".fg(Color::Yellow)),
                    Some(Color::Yellow),
                )
            ]);
    }
    if let Some(location) = comment.find("BUG") {
        let begin = comment_location.pos + location;
        let end = begin + 3;
        let warn_span = FileSpan {
            begin: Coordinate {
                file: Rc::clone(&file),
                pos: begin
            },
            end: Coordinate {
                file: Rc::clone(&file),
                pos: end
            }
        };
        advice(
            comment_location.clone(),
            ErrorCode::FixmeFoundInComment,
            format!("'{}' found inside a comment, maybe you should look into this", "BUG".fg(Color::Yellow)),
            Some(format!(
                "Cheese looks for keywords like '{}' in comments, because these keywords generally signify that code is incomplete or could be buggy, the compiler option --no-comment-advice disables this.",
                "BUG".fg(Color::Yellow))),
            vec![
                ReportLabel::new(
                    warn_span,
                    format!("'{}' found here", "BUG".fg(Color::Yellow)),
                    Some(Color::Yellow),
                )
            ]);
    }
    if let Some(location) = comment.find("XXX") {
        let begin = comment_location.pos + location;
        let end = begin + 3;
        let warn_span = FileSpan {
            begin: Coordinate {
                file: Rc::clone(&file),
                pos: begin
            },
            end: Coordinate {
                file: Rc::clone(&file),
                pos: end
            }
        };
        advice(
            comment_location.clone(),
            ErrorCode::FixmeFoundInComment,
            format!("'{}' found inside a comment, maybe you should look into this", "XXX".fg(Color::Yellow)),
            Some(format!(
                "Cheese looks for keywords like '{}' in comments, because these keywords generally signify that code is incomplete or could be buggy, the compiler option --no-comment-advice disables this.",
                "XXX".fg(Color::Yellow))),
            vec![
                ReportLabel::new(
                    warn_span,
                    format!("'{}' found here", "XXX".fg(Color::Yellow)),
                    Some(Color::Yellow),
                )
            ]);
    }
    if let Some(location) = comment.find("HACK") {
        let begin = comment_location.pos + location;
        let end = begin + 4;
        let warn_span = FileSpan {
            begin: Coordinate {
                file: Rc::clone(&file),
                pos: begin
            },
            end: Coordinate {
                file: Rc::clone(&file),
                pos: end
            }
        };
        advice(
            comment_location.clone(),
            ErrorCode::FixmeFoundInComment,
            format!("'{}' found inside a comment, maybe you should look into this", "HACK".fg(Color::Yellow)),
            Some(format!(
                "Cheese looks for keywords like '{}' in comments, because these keywords generally signify that code is incomplete or could be buggy, the compiler option --no-comment-advice disables this.",
                "HACK".fg(Color::Yellow))),
            vec![
                ReportLabel::new(
                    warn_span,
                    format!("'{}' found here", "HACK".fg(Color::Yellow)),
                    Some(Color::Yellow),
                )
            ]);
    }
}



pub fn add(left: usize, right: usize) -> usize {
    left + right
}


// Lexer tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
