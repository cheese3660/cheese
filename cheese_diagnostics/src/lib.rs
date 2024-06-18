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
    IncorrectSeparator = PARSER,
    ExpectedStructureStatement,
    ExpectedImportPath,
    ExpectedImportName,
    ExpectedFunctionName,
    ExpectedArgumentDeclarationOrClose,
    ExpectedCommaOrClose,
    ExpectedArgumentName,
    ExpectedColon,
    ExpectedOpeningParentheses,
    ExpectedClosingDiamond,
    ExpectedIdentifierOrOpeningDiamond,
    ExpectedType,
    ExpectedFunctionInformation,
    ExpectedSemicolon,
    UnexpectedGenerics,
    ExpectedSliceTypeClose,
    ExpectedArgumentClose,
    ExpectedFieldName,
    ExpectedPrimary,
    InvalidCharacterLiteral,
    ExpectedEnumId,
    ExpectedMatchingParentheses,
    ExpectedBlockName,
    ExpectedCaptureSpecifier,
    ExpectedCaptureName,
    ExpectedCaptureClose,
    ExpectedReturnType,
    ExpectedIsOrGeneric,
    ExpectedClosingBrace,
    ExpectedEquals,
    ExpectedVariableName,
    ExpectedOpeningBrace,
    ExpectedMatchBody,
    ExpectedCommaOrArrow,
    ExpectedClosingBracket,


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
    pub fn new(span: FileSpan, message: String, color: Option<Color>) -> ReportLabel {
        ReportLabel {
            span,
            message,
            color,
        }
    }
}


fn base_report(kind: ReportKind, coordinate: Coordinate, error_code: ErrorCode, message: String, note: Option<String>, labels: Vec<ReportLabel>) {
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


pub fn warn(coordinate: Coordinate, error_code: ErrorCode, message: String, note: Option<String>, labels: Vec<ReportLabel>) {
    base_report(Warning,coordinate,error_code,message,note,labels);
}

pub fn error(coordinate: Coordinate, error_code: ErrorCode, message: String, note: Option<String>, labels: Vec<ReportLabel>) {
    base_report(Error,coordinate,error_code,message,note,labels);
}

pub fn exiting_error(coordinate: Coordinate, error_code: ErrorCode, message: String, note: Option<String>, labels: Vec<ReportLabel>) {
    error(coordinate,error_code,message,note,labels);
    exit(error_code.get_code() as i32);
}

pub fn advice(coordinate: Coordinate, error_code: ErrorCode, message: String, note: Option<String>, labels: Vec<ReportLabel>) {
    base_report(Advice,coordinate,error_code,message,note,labels);
}