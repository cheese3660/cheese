pub mod locating;

use std::process::exit;
use ariadne::{Cache, Color, Label, Report, ReportKind};
use ariadne::ReportKind::{Advice, Error, Warning};
use crate::locating::{Coordinate, FileSpan};

// Codes

const LEXER: isize = 1000;
const PARSER: isize = 2000;
const WARNING: isize = 500;

const ADVICE: isize = 800;

#[derive(Copy, Clone)]
pub enum ErrorCode {
    // Lexer errors
    UnterminatedBlockComment = LEXER,
    UnterminatedStringLiteral,
    UnterminatedCharacterLiteral,
    UnexpectedCharacter,
    ExpectedSelf,

    // Lexer Advice
    TodoFoundInComment = LEXER + ADVICE,
    FixmeFoundInComment,
    BugFoundInComment,
    XXXFoundInComment,
    HackFoundInComment,

    // Parser errors
    ExpectedModuleStatement = PARSER,
    ExpectedImportPath,
    ExpectedSemicolon,
    UnexpectedEndOfFile,
    ExpectedEndOfFile,
    UnexpectedFlags,
    ExpectedCommaOrClose,
    ExpectedName,
    ExpectedEquals,
    ExpectedArgumentClose,
    ExpectedFieldName,
    ExpectedColon,
    ExpectedIdentifierOrOpeningDiamond,
    ExpectedPrimary,
    ExpectedFn,
    ExpectedOpenParentheses,
    ExpectedCloseParentheses,
    ExpectedClose,
    ExpectedSliceTypeClose,
    ExpectedType,
    InvalidCharacterLiteral,
    ExpectedExpressionName,
    ExpectedEnumId,
    ExpectedCaptureSpecifier,
    ExpectedCaptureName,
    ExpectedMatchBody,
    ExpectedCommaOrArrow,
    ExpectedOpenBrace,

    // Parser advice
    AbleToSimplifyGenerics = PARSER + ADVICE,



    NotImplemented = 9998,
    GeneralCompilerError = 9999
}


impl ErrorCode {
    pub fn get_code(self) -> u32 {
        self as u32
    }
}

/// Automatically generates a source file, used for error report buildings

pub struct ReportLabel {
    span: FileSpan,
    message: String,
    color: Option<Color>,
}

impl ReportLabel {
    pub fn new<T: ToString>(span: FileSpan, message: T, color: Option<Color>) -> ReportLabel {
        ReportLabel {
            span,
            message: message.to_string(),
            color,
        }
    }
}


fn base_report<T: ToString, U: ToString>(kind: ReportKind, coordinate: Coordinate, error_code: ErrorCode, message: T, note: Option<U>, labels: Vec<ReportLabel>) {
    let file = coordinate.file;
    let mut builder = Report::build(kind, file.filename.as_str(), coordinate.pos);
    builder = builder.with_code(error_code.get_code());
    builder = builder.with_message(message);
    for label in labels {
        match label.color {
            Some(col) =>
                builder = builder.with_label(Label::new((file.filename.as_str(), label.span.begin.pos..label.span.end.pos)).with_message(label.message).with_color(col)),
            None =>
                builder = builder.with_label(Label::new((file.filename.as_str(), label.span.begin.pos..label.span.end.pos)).with_message(label.message))
        };
    }
    match note {
        None => {}
        Some(n) => builder = builder.with_note(n)
    }
    builder.finish().eprint((file.filename.as_str(),file.source.clone())).unwrap()
}


pub fn warn<T: ToString, U: ToString>(coordinate: Coordinate, error_code: ErrorCode, message: T, note: Option<U>, labels: Vec<ReportLabel>) {
    base_report(Warning,coordinate,error_code,message,note,labels);
}

pub fn error<T: ToString, U: ToString>(coordinate: Coordinate, error_code: ErrorCode, message: T, note: Option<U>, labels: Vec<ReportLabel>) {
    base_report(Error,coordinate,error_code,message,note,labels);
}

pub fn exiting_error<T: ToString, U: ToString>(coordinate: Coordinate, error_code: ErrorCode, message: T, note: Option<U>, labels: Vec<ReportLabel>) {
    error(coordinate,error_code,message,note,labels);
    exit(error_code.get_code() as i32);
}

pub fn advice<T: ToString, U: ToString>(coordinate: Coordinate, error_code: ErrorCode, message: T, note: Option<U>, labels: Vec<ReportLabel>) {
    base_report(Advice,coordinate,error_code,message,note,labels);
}