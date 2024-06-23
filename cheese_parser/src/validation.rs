use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use ariadne::Color;
use num_bigint::BigInt;
use paste::paste;
use cheese_diagnostics::ErrorCode;
use cheese_utilities::strings::Escapable;
use cheese_utilities::trees::{DisplayableTree, DisplayNode, NodeBuilder};
use crate::ast::{AstNode, AstNodeData, DeclarationFlags, Operator};

type NodePtr = Box<AstValidator>;
type NodeList = Vec<NodePtr>;
type NodeDict = HashMap<String,NodePtr>;

type OptionalNode = Option<NodePtr>;

#[derive(Debug,Clone)]
pub enum AstValidator {
    Any,
    Bool,
    SignedSize,
    UnsignedSize,
    Float32,
    Float64,
    Complex32,
    Complex64,
    Opaque,
    Void,
    CompileTimeFloat,
    CompileTimeComplex,
    CompileTimeString,
    CompileTimeInteger,
    Type,
    NoReturn,
    True,
    False,
    None,
    Underscore,
    Continue,
    EmptyBreak,
    EmptyReturn,
    SelfValue,
    ConstSelfValue,
    SelfType,
    ConstReferenceImplicitCapture,
    ReferenceImplicitCapture,
    CopyImplicitCapture,
    MatchAll,
    NonExhaustive,
    UnknownSize,
    InferredSize,
    SignedIntegerType(u64),
    UnsignedIntegerType(u64),
    StringLiteral(String),
    FloatLiteral(f64),
    ImaginaryLiteral(f64),
    IntegerLiteral(BigInt),
    NameReference(String),
    UnnamedBlock(NodeList),
    TupleLiteral(NodeList),
    ArrayLiteral(NodeList),
    EnumLiteral(String),
    BuiltinReference(String),
    CopyCapture(String),
    ReferenceCapture(String),
    ConstantReferenceCapture(String),
    ObjectLiteral(NodeList),
    Block(NodeList),
    Break(NodePtr),
    Return(NodePtr),
    Yield(NodePtr),
    Not(NodePtr),
    UnaryMinus(NodePtr),
    UnaryPlus(NodePtr),
    Dereference(NodePtr),
    AddressOf(NodePtr),
    Concept(NodePtr),
    Loop(NodePtr),
    FilterTransformation(NodePtr),
    MapTransformation(NodePtr),
    Comptime(NodePtr),
    ImplicitResult(NodePtr),
    Typeof(NodePtr),
    ImplicitArray {
        constant: bool,
        subtype: NodePtr,
    },
    Slice {
        constant: bool,
        subtype: NodePtr,
    },
    Reference {
        constant: bool,
        subtype: NodePtr,
    },
    TupleCall {
        functional: NodePtr,
        args: NodeList,
    },
    ArrayCall {
        functional: NodePtr,
        args: NodeList,
    },
    NamedBlock {
        name: String,
        body: NodeList,
    },
    NamedBreak {
        name: String,
        value: NodePtr,
    },
    ObjectCall {
        functional: NodePtr,
        args: NodeList,
    },
    Match {
        value: NodePtr,
        arms: NodeList,
    },
    MatchRange {
        begin: NodePtr,
        end: NodePtr,
    },
    DestructuringMatchArm {
        matches: NodeList,
        store: OptionalNode,
    },
    MatchEnumStructure {
        enum_identifier: String,
        children: NodeDict,
    },
    MatchEnumTuple {
        enum_identifier: String,
        children: NodeList,
    },
    MatchValue(NodePtr),
    MatchConstraint(NodePtr),
    DestructuringMatchStructure(NodeDict),
    DestructuringMatchTuple(NodeList),
    DestructuringMatchArray(NodeList),
    Enum {
        containing_type: OptionalNode,
        children: NodeList,
    },
    FieldLiteral {
        name: String,
        value: NodePtr,
    },
    Subscription {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Multiplication {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Division {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Modulus {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Addition {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Subtraction {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    LeftShift {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    RightShift {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    LesserThan {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    GreaterThan {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    LesserEqual {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    GreaterEqual {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    EqualTo {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    NotEqualTo {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    And {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Or {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Xor {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Combine {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Reassign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    Assign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    AddAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    SubtractAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    MultiplyAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    DivideAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    ModulateAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    ShiftLeftAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    ShiftRightAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    AndAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    OrAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    XorAssign {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    IsType {
        lhs: NodePtr,
        rhs: NodePtr,
    },
    DynamicCast {
        lhs: NodePtr,
        to: NodePtr,
    },
    Cast {
        lhs: NodePtr,
        to: NodePtr,
    },
    Range {
        begin: NodePtr,
        end: NodePtr,
    },
    TypeDeclaration {
        flags: DeclarationFlags,
        name: String,
        generic_arguments: Option<NodeList>,
        alias: NodePtr,
    },
    Field {
        flags: DeclarationFlags,
        name: Option<String>,
        field_type: NodePtr,
    },
    Argument {
        name: Option<String>,
        argument_type: NodePtr,
    },
    Import {
        path: String,
        name: String,
    },
    Structure {
        is_tuple: bool,
        interfaces: NodeList,
        children: NodeList,
    },
    FunctionPrototype {
        flags: DeclarationFlags,
        name: String,
        arguments: NodeList,
        return_type: OptionalNode,
    },
    FunctionImport {
        flags: DeclarationFlags,
        name: String,
        arguments: NodeList,
        return_type: OptionalNode,
        import_name: OptionalNode,
    },
    Function {
        flags: DeclarationFlags,
        name: String,
        generic_arguments: Option<NodeList>,
        arguments: NodeList,
        return_type: OptionalNode,
        body: NodePtr,
    },
    Operator {
        flags: DeclarationFlags,
        operator: Operator,
        generic_arguments: Option<NodeList>,
        arguments: NodeList,
        return_type: OptionalNode,
        body: NodePtr,
    },
    VariableDeclaration {
        definition: NodePtr,
        value: NodePtr,
    },
    VariableDefinition {
        flags: DeclarationFlags,
        name: String,
        variable_type: OptionalNode,
    },
    Closure {
        arguments: NodeList,
        captures: NodeList,
        return_type: OptionalNode,
        body: NodePtr,
    },
    AnonymousFunction {
        flags: DeclarationFlags,
        arguments: NodeList,
        return_type: OptionalNode,
        body: NodePtr,
    },
    FunctionType {
        flags: DeclarationFlags,
        arguments: NodeList,
        return_type: NodePtr,
    },
    StructureDestructure(NodeDict),
    TupleDestructure(NodeList),
    ArrayDestructure(NodeList),
    SliceDestructure(NodeList),
    Destructure {
        structure: NodePtr,
        value: NodePtr,
    },
    Interface {
        interfaces: NodeList,
        children: NodeList,
        dynamic: bool,
    },
    If {
        condition: NodePtr,
        unwrap: OptionalNode,
        body: NodePtr,
        else_statement: OptionalNode,
    },
    ComptimeIf {
        condition: NodePtr,
        body: NodePtr,
        else_statement: OptionalNode,
    },
    While {
        condition: NodePtr,
        body: NodePtr,
        else_statement: OptionalNode,
    },
    For {
        capture: NodePtr,
        index_capture: OptionalNode,
        iterable: NodePtr,
        transformations: NodeList,
        body: NodePtr,
        else_statement: OptionalNode,
    },
    MatchArm {
        matches: NodeList,
        store: OptionalNode,
        body: NodePtr,
    },
    EnumMember {
        name: String,
        tuple: bool,
        children: NodeList,
        value: OptionalNode,
    },
    ArrayType {
        constant: bool,
        dimensions: NodeList,
        child: NodePtr,
    },
    TypeMemberReference {
        referee: NodePtr,
        member: String
    },
    GenericInstanceReference {
        referee: NodePtr,
        generic_args: NodeList
    }
}
impl Display for AstValidator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let tree = self.to_node();
        std::fmt::Display::fmt(&tree, f)
    }
}

impl AstValidator {
    // Now this is going to be a long mess
    pub fn validate(&self, node: &crate::ast::NodePtr) -> bool {
        let data = &node.data;
        macro_rules! check {
            ($name:ident) => {
                if let AstNodeData::$name = data { true } else { false }
            };
            ($name:ident, $value:expr) => {
                if let AstNodeData::$name(v) = data { v == $value } else { false }
            };
            ($name:ident, $lhs:expr, $rhs:expr) => {
                check!($name,lhs,$lhs,rhs,$rhs)
            };
            ($name:ident, $lhs_name:ident, $lhs:expr, $rhs_name:ident, $rhs:expr) => {
                if let AstNodeData::$name{
                    $lhs_name: a,
                    $rhs_name: b
                } = &data {
                    $lhs.validate(a) && $rhs.validate(b)
                } else {
                    false
                }
            }
        }
        macro_rules! check_list {
            ($name: ident, $validators:expr) => {
                if let AstNodeData::$name(v) = &data { validate_list(v,$validators)} else { false }
            }
        }
        macro_rules! check_dict {
            ($name: ident, $validators:expr) => {
                if let AstNodeData::$name(v) = &data { validate_dict(v,$validators)} else { false }
            };
        }
        macro_rules! check_unary {
            ($name: ident, $validator:expr) => {
                if let AstNodeData::$name(v) = &data { $validator.validate(v) } else { false }
            };
        }
        let result = match self {
            AstValidator::Any => true,
            AstValidator::Bool => check!(Bool),
            AstValidator::SignedSize => check!(SignedSize),
            AstValidator::UnsignedSize => check!(UnsignedSize),
            AstValidator::Float32 => check!(Float32),
            AstValidator::Float64 => check!(Float64),
            AstValidator::Complex32 => check!(Complex32),
            AstValidator::Complex64 => check!(Complex64),
            AstValidator::Opaque => check!(Opaque),
            AstValidator::Void => check!(Void),
            AstValidator::CompileTimeFloat => check!(CompileTimeFloat),
            AstValidator::CompileTimeComplex => check!(CompileTimeComplex),
            AstValidator::CompileTimeString => check!(CompileTimeString),
            AstValidator::CompileTimeInteger => check!(CompileTimeInteger),
            AstValidator::Type => check!(Type),
            AstValidator::NoReturn => check!(NoReturn),
            AstValidator::True => check!(True),
            AstValidator::False => check!(False),
            AstValidator::None => check!(None),
            AstValidator::Underscore => check!(Underscore),
            AstValidator::Continue => check!(Continue),
            AstValidator::EmptyBreak => check!(EmptyBreak),
            AstValidator::EmptyReturn => check!(EmptyReturn),
            AstValidator::SelfValue => check!(SelfValue),
            AstValidator::ConstSelfValue => check!(ConstSelfValue),
            AstValidator::SelfType => check!(SelfType),
            AstValidator::ConstReferenceImplicitCapture => check!(ConstReferenceImplicitCapture),
            AstValidator::ReferenceImplicitCapture => check!(ReferenceImplicitCapture),
            AstValidator::CopyImplicitCapture => check!(CopyImplicitCapture),
            AstValidator::MatchAll => check!(MatchAll),
            AstValidator::NonExhaustive => check!(NonExhaustive),
            AstValidator::UnknownSize => check!(UnknownSize),
            AstValidator::InferredSize => check!(InferredSize),
            AstValidator::SignedIntegerType(sz) => check!(SignedIntegerType,sz),
            AstValidator::UnsignedIntegerType(sz) => check!(UnsignedIntegerType,sz),
            AstValidator::StringLiteral(s) => check!(StringLiteral,s),
            AstValidator::FloatLiteral(f) => check!(FloatLiteral,f),
            AstValidator::ImaginaryLiteral(i) => check!(ImaginaryLiteral, i),
            AstValidator::IntegerLiteral(i) => if let AstNodeData::IntegerLiteral(other) = data {
                other.eq(i)
            } else {
                false
            },
            AstValidator::NameReference(name) => check!(NameReference, name),
            AstValidator::UnnamedBlock(block) => check_list!(UnnamedBlock, block),
            AstValidator::TupleLiteral(args) => check_list!(TupleLiteral, args),
            AstValidator::ArrayLiteral(args) => check_list!(ArrayLiteral, args),
            AstValidator::EnumLiteral(name) => check!(EnumLiteral, name),
            AstValidator::BuiltinReference(name) => check!(BuiltinReference, name),
            AstValidator::CopyCapture(name) => check!(CopyCapture, name),
            AstValidator::ReferenceCapture(name) => check!(ReferenceCapture, name),
            AstValidator::ConstantReferenceCapture(name) => check!(ConstantReferenceCapture, name),
            AstValidator::ObjectLiteral(args) => check_list!(ObjectLiteral,args),
            AstValidator::Block(list) => check_list!(Block, list),
            AstValidator::Break(arg) => check_unary!(Break, arg),
            AstValidator::Return(arg) => check_unary!(Return, arg),
            AstValidator::Yield(arg) => check_unary!(Yield, arg),
            AstValidator::Not(arg) => check_unary!(Not, arg),
            AstValidator::UnaryMinus(arg) => check_unary!(UnaryMinus, arg),
            AstValidator::UnaryPlus(arg) => check_unary!(UnaryPlus, arg),
            AstValidator::Dereference(arg) => check_unary!(Dereference, arg),
            AstValidator::AddressOf(arg) => check_unary!(AddressOf, arg),
            AstValidator::Concept(arg) => check_unary!(Concept, arg),
            AstValidator::Loop(arg) => check_unary!(Loop, arg),
            AstValidator::FilterTransformation(arg) => check_unary!(FilterTransformation, arg),
            AstValidator::MapTransformation(arg) => check_unary!(MapTransformation, arg),
            AstValidator::Comptime(arg) => check_unary!(Comptime, arg),
            AstValidator::ImplicitResult(arg) => check_unary!(ImplicitResult,arg),
            AstValidator::Typeof(arg) => check_unary!(Typeof,arg),
            AstValidator::ImplicitArray {
                constant, subtype
            } => {
                if let AstNodeData::ImplicitArray { constant: c2, subtype: s2 } = &data {
                    *constant == *c2 && subtype.validate(s2)
                } else {
                    false
                }
            },
            AstValidator::Slice {
                constant, subtype
            } => {
                if let AstNodeData::Slice { constant: c2, subtype: s2 } = &data {
                    *constant == *c2 && subtype.validate(s2)
                } else {
                    false
                }
            },
            AstValidator::Reference {
                constant, subtype
            } => {
                if let AstNodeData::Reference { constant: c2, subtype: s2 } = &data {
                    *constant == *c2 && subtype.validate(s2)
                } else {
                    false
                }
            },
            AstValidator::TupleCall { functional, args } => {
                if let AstNodeData::TupleCall {
                    functional: f2,
                    args: a2
                } = &data {
                    functional.validate(f2) && validate_list(a2, args)
                } else {
                    false
                }
            },
            AstValidator::ArrayCall { functional, args } => {
                if let AstNodeData::ArrayCall {
                    functional: f2,
                    args: a2
                } = &data {
                    functional.validate(f2) && validate_list(a2, args)
                } else {
                    false
                }
            },
            AstValidator::NamedBlock { name, body } => {
                if let AstNodeData::NamedBlock {
                    name: n2,
                    body: b2,
                } = &data {
                    name == n2 && validate_list(b2, body)
                } else {
                    false
                }
            },
            AstValidator::NamedBreak { name, value } => {
                if let AstNodeData::NamedBreak {
                    name: n2,
                    value: v2
                } = &data {
                    name == n2 && value.validate(v2)
                } else {
                    false
                }
            },
            AstValidator::ObjectCall { functional, args } => {
                if let AstNodeData::ObjectCall {
                    functional: f2,
                    args: a2
                } = &data {
                    functional.validate(f2) && validate_list(a2, args)
                } else {
                    false
                }
            },
            AstValidator::Match { value, arms } => {
                if let AstNodeData::Match {
                    value: v2,
                    arms: a2
                } = &data {
                    value.validate(v2) && validate_list(a2, arms)
                } else {
                    false
                }
            },
            AstValidator::MatchRange { begin, end } => check!(MatchRange,begin,begin,end,end),
            AstValidator::DestructuringMatchArm { matches, store } => {
                if let AstNodeData::DestructuringMatchArm {
                    matches: m2,
                    store: s2
                } = &data {
                    validate_optional(s2,store) && validate_list(m2, matches)
                } else {
                    false
                }
            },
            AstValidator::MatchEnumStructure { enum_identifier, children } => {
                if let AstNodeData::MatchEnumStructure {
                    enum_identifier: e2,
                    children: c2
                } = &data {
                    e2 == enum_identifier && validate_dict(c2,children)
                } else {
                    false
                }
            },
            AstValidator::MatchEnumTuple { enum_identifier, children }  => {
                if let AstNodeData::MatchEnumTuple {
                    enum_identifier: e2,
                    children: c2
                } = &data {
                    e2 == enum_identifier && validate_list(c2,children)
                } else {
                    false
                }
            },
            AstValidator::MatchValue(value) => check_unary!(MatchValue,value),
            AstValidator::MatchConstraint(value) => check_unary!(MatchConstraint,value),
            AstValidator::DestructuringMatchStructure(value) => check_dict!(DestructuringMatchStructure,value),
            AstValidator::DestructuringMatchTuple(value) => check_list!(DestructuringMatchTuple,value),
            AstValidator::DestructuringMatchArray(value) => check_list!(DestructuringMatchArray,value),
            AstValidator::Enum { containing_type, children } => {
                if let AstNodeData::Enum {
                    containing_type: ct2,
                    children: c2
                } = &data {
                    validate_optional(ct2,containing_type) && validate_list(c2, children)
                } else {
                    false
                }
            }
            AstValidator::FieldLiteral { name, value } => {
                if let AstNodeData::FieldLiteral {
                    name: n2,
                    value: v2
                } = &data {
                    name == n2 && value.validate(v2)
                } else {
                    false
                }
            }
            AstValidator::Subscription { lhs, rhs } => check!(Subscription, lhs, rhs),
            AstValidator::Multiplication { lhs, rhs } => check!(Multiplication, lhs, rhs),
            AstValidator::Division { lhs, rhs } => check!(Division, lhs, rhs),
            AstValidator::Modulus { lhs, rhs } => check!(Modulus, lhs, rhs),
            AstValidator::Addition { lhs, rhs } => check!(Addition, lhs, rhs),
            AstValidator::Subtraction { lhs, rhs } => check!(Subtraction, lhs, rhs),
            AstValidator::LeftShift { lhs, rhs } => check!(LeftShift, lhs, rhs),
            AstValidator::RightShift { lhs, rhs } => check!(RightShift, lhs, rhs),
            AstValidator::LesserThan { lhs, rhs } => check!(LesserThan, lhs, rhs),
            AstValidator::GreaterThan { lhs, rhs } => check!(GreaterThan, lhs, rhs),
            AstValidator::LesserEqual { lhs, rhs } => check!(LesserEqual, lhs, rhs),
            AstValidator::GreaterEqual { lhs, rhs } => check!(GreaterEqual, lhs, rhs),
            AstValidator::EqualTo { lhs, rhs } => check!(EqualTo, lhs, rhs),
            AstValidator::NotEqualTo { lhs, rhs } => check!(NotEqualTo, lhs, rhs),
            AstValidator::And { lhs, rhs } => check!(And, lhs, rhs),
            AstValidator::Or { lhs, rhs } => check!(Or, lhs, rhs),
            AstValidator::Xor { lhs, rhs } => check!(Xor, lhs, rhs),
            AstValidator::Combine { lhs, rhs } => check!(Combine, lhs, rhs),
            AstValidator::Reassign { lhs, rhs } => check!(Reassign, lhs, rhs),
            AstValidator::Assign { lhs, rhs } => check!(Assign, lhs, rhs),
            AstValidator::AddAssign { lhs, rhs } => check!(AddAssign, lhs, rhs),
            AstValidator::SubtractAssign { lhs, rhs } => check!(SubtractAssign, lhs, rhs),
            AstValidator::MultiplyAssign { lhs, rhs } => check!(MultiplyAssign, lhs, rhs),
            AstValidator::DivideAssign { lhs, rhs } => check!(DivideAssign, lhs, rhs),
            AstValidator::ModulateAssign { lhs, rhs } => check!(ModulateAssign, lhs, rhs),
            AstValidator::ShiftLeftAssign { lhs, rhs } => check!(ShiftLeftAssign, lhs, rhs),
            AstValidator::ShiftRightAssign { lhs, rhs } => check!(ShiftRightAssign, lhs, rhs),
            AstValidator::AndAssign { lhs, rhs } => check!(AndAssign, lhs, rhs),
            AstValidator::OrAssign { lhs, rhs } => check!(OrAssign, lhs, rhs),
            AstValidator::XorAssign { lhs, rhs } => check!(XorAssign, lhs, rhs),
            AstValidator::IsType { lhs, rhs } => check!(SubtractAssign, lhs, rhs),
            AstValidator::DynamicCast { lhs, to } => check!(DynamicCast, lhs, lhs, to, to),
            AstValidator::Cast { lhs, to } => check!(Cast, lhs, lhs, to, to),
            AstValidator::Range { begin, end } => check!(Range, begin, begin, end, end),
            AstValidator::TypeDeclaration { flags, name, generic_arguments, alias } => {
                if let AstNodeData::TypeDeclaration {
                    flags: f2,
                    name: n2,
                    generic_arguments: g2,
                    alias: a2
                } = &data {
                    f2 == flags && name == n2 && match generic_arguments {
                        None => g2.is_none(),
                        Some(v) => match g2 {
                            None => false,
                            Some(v2) => validate_list(v2, v)
                        }
                    } && alias.validate(a2)
                } else {
                    false
                }
            }
            AstValidator::Field { flags, name, field_type } => {
                if let AstNodeData::Field {
                    flags: f2,
                    name: n2,
                    field_type: ft2
                } = &data {
                    f2 == flags && name == n2 && field_type.validate(ft2)
                } else {
                    false
                }
            }
            AstValidator::Argument { name, argument_type } => {
                if let AstNodeData::Argument {
                    name: n2, argument_type: a2
                } = &data {
                    name == n2 && argument_type.validate(a2)
                } else {
                    false
                }
            }
            AstValidator::Import { path, name } => {
                if let AstNodeData::Import {
                    name: n2,
                    path: p2
                } = &data {
                    name == n2 && path == p2
                } else {
                    false
                }
            }
            AstValidator::Structure { is_tuple, interfaces, children } => {
                if let AstNodeData::Structure {
                    is_tuple: t2,
                    interfaces: i2,
                    children: c2
                } = &data {
                    t2 == is_tuple && validate_list(i2,interfaces) && validate_list(c2,children)
                } else {
                    false
                }
            }
            AstValidator::FunctionPrototype { flags, name, arguments, return_type } => {
                if let AstNodeData::FunctionPrototype {
                    flags: f2,
                    name: n2,
                    arguments: a2,
                    return_type: r2
                } = &data {
                    f2 == flags && name == n2  && validate_list(a2,arguments) && validate_optional(r2, return_type)
                } else {
                    false
                }
            }
            AstValidator::FunctionImport { flags, name, arguments, return_type, import_name } => {
                if let AstNodeData::FunctionImport {
                    flags: f2,
                    name: n2,
                    arguments: a2,
                    return_type: r2,
                    import_name: i2
                } = &data {
                    f2 == flags && name == n2 && validate_list(a2,arguments) && validate_optional(r2, return_type) && validate_optional(i2, import_name)
                } else {
                    false
                }
            }
            AstValidator::Function { flags, name, generic_arguments, arguments, return_type, body } => {
                if let AstNodeData::Function {
                    flags: f2,
                    name: n2,
                    generic_arguments: g2,
                    arguments: a2,
                    return_type: r2,
                    body: b2
                } = &data {
                    f2 == flags && name == n2 && match generic_arguments {
                        None => g2.is_none(),
                        Some(v) => match g2 {
                            None => false,
                            Some(v2) => validate_list(v2, v)
                        }
                    } && validate_list(a2,arguments) && validate_optional(r2, return_type) && body.validate(b2)
                } else {
                    false
                }
            }
            AstValidator::Operator { flags, operator, generic_arguments, arguments, return_type, body } => {
                if let AstNodeData::Operator {
                    flags: f2,
                    operator: o2,
                    generic_arguments: g2,
                    arguments: a2,
                    return_type: r2,
                    body: b2
                } = &data {
                    f2 == flags && operator == o2 && match generic_arguments {
                        None => g2.is_none(),
                        Some(v) => match g2 {
                            None => false,
                            Some(v2) => validate_list(v2, v)
                        }
                    } && validate_list(a2,arguments) && validate_optional(r2, return_type) && body.validate(b2)
                } else {
                    false
                }
            }
            AstValidator::VariableDeclaration { definition, value } => check!(VariableDeclaration,definition,definition,value,value),
            AstValidator::VariableDefinition { flags, name, variable_type } => {
                if let AstNodeData::VariableDefinition {
                    flags: f2,
                    name: n2,
                    variable_type: v2
                } = &data {
                    flags == f2 && name == n2 && validate_optional(v2, variable_type)
                } else {
                    false
                }
            }
            AstValidator::Closure { arguments, captures, return_type, body } => {
                if let AstNodeData::Closure {
                    arguments: a2,
                    captures: c2,
                    return_type: r2,
                    body: b2
                } = &data {
                    validate_list(a2,arguments) && validate_list(c2, captures) && validate_optional(r2, return_type) && body.validate(b2)
                } else {
                    false
                }
            }
            AstValidator::AnonymousFunction { flags, arguments, return_type, body } => {
                if let AstNodeData::AnonymousFunction {
                    flags: f2,
                    arguments: a2,
                    return_type: r2,
                    body: b2
                } = &data {
                    f2 == flags && validate_list(a2,arguments) && validate_optional(r2, return_type) && body.validate(b2)
                } else {
                    false
                }
            }
            AstValidator::FunctionType { flags, arguments, return_type } => {
                if let AstNodeData::FunctionType {
                    flags: f2,
                    arguments: a2,
                    return_type: r2,
                } = &data {
                    f2 == flags && validate_list(a2,arguments) && return_type.validate(r2)
                } else {
                    false
                }
            }
            AstValidator::StructureDestructure(list) => check_dict!(StructureDestructure, list),
            AstValidator::TupleDestructure(list) => check_list!(TupleDestructure, list),
            AstValidator::ArrayDestructure(list) => check_list!(ArrayDestructure, list),
            AstValidator::SliceDestructure(list) => check_list!(SliceDestructure, list),
            AstValidator::Destructure { structure, value } => check!(Destructure,structure,structure,value,value),
            AstValidator::Interface { interfaces, children, dynamic } => {
                if let AstNodeData::Interface {
                    interfaces: i2, children: c2, dynamic: d2
                } = &data {
                    d2 == dynamic && validate_list(i2,interfaces) && validate_list(c2, children)
                } else {
                    false
                }
            }
            AstValidator::If { condition, unwrap, body, else_statement } => {
                if let AstNodeData::If {
                    condition: c2, unwrap: u2, body: b2, else_statement: e2
                } = &data {
                    condition.validate(c2) && validate_optional(u2, unwrap) && body.validate(b2) && validate_optional(e2, else_statement)
                } else {
                    false
                }
            }
            AstValidator::ComptimeIf { condition, body, else_statement } => {
                if let AstNodeData::ComptimeIf {
                    condition: c2, body: b2, else_statement: e2
                } = &data {
                    condition.validate(c2) && body.validate(b2) && validate_optional(e2, else_statement)
                } else {
                    false
                }
            }
            AstValidator::While { condition, body, else_statement } => {
                if let AstNodeData::While {
                    condition: c2, body: b2, else_statement: e2
                } = &data {
                    condition.validate(c2) && body.validate(b2) && validate_optional(e2, else_statement)
                } else {
                    false
                }
            }
            AstValidator::For { capture, index_capture, iterable, transformations, body, else_statement } => {
                if let AstNodeData::For {
                    capture:c2, index_capture:ic2, iterable:i2, transformations:t2, body:b2, else_statement:e2
                } = &data {
                    capture.validate(c2) && validate_optional(ic2, index_capture) && iterable.validate(i2) && validate_list(t2, transformations) && body.validate(b2) && validate_optional(e2, else_statement)
                } else {
                    false
                }
            }
            AstValidator::MatchArm { matches, store, body } => {
                if let AstNodeData::MatchArm {
                    matches: m2, store: s2, body: b2
                } = &data {
                    validate_list(m2, matches) && validate_optional(s2, store) && body.validate(b2)
                } else {
                    false
                }
            }
            AstValidator::EnumMember { name, tuple, children, value } => {
                if let AstNodeData::EnumMember {
                    name: n2, tuple: t2, children: c2, value: v2
                } = &data {
                    tuple == t2 && name == n2 && validate_list(c2, children) && validate_optional(v2, value)
                } else {
                    false
                }
            }
            AstValidator::ArrayType { constant, dimensions, child } => {
                if let AstNodeData::ArrayType {
                    constant:c2, dimensions: d2, child: c3
                } = &data {
                    constant == c2 && validate_list(d2, dimensions) && child.validate(c3)
                } else {
                    false
                }
            }
            AstValidator::TypeMemberReference { referee, member } => {
                if let AstNodeData::TypeMemberReference {
                    referee: r2, member: m2
                } = &data {
                    m2 == member && referee.validate(r2)
                } else {
                    false
                }
            }
            AstValidator::GenericInstanceReference { referee, generic_args } => {
                if let AstNodeData::GenericInstanceReference { referee: r2, generic_args: g2 } = &data {
                    referee.validate(r2) && validate_list(g2, generic_args)
                } else {
                    false
                }
            }
        };
        if !result {
            println!("failed to validate!\nexpected:\n{self}\n\ngot:\n{node}\n\n")
        }
        result
    }
}
const TYPE_COLOR: Color = Color::Red;
const VALUE_COLOR: Color = Color::Magenta;
const NAME_COLOR: Color = Color::Cyan;
const KEYWORD_COLOR: Color = Color::Blue;
const METHOD_COLOR: Color = Color::BrightYellow;
const OTHER_COLOR: Color = Color::Green;
impl DisplayableTree for AstValidator {
    fn to_node(&self) -> Box<DisplayNode> {
        match self {
            AstValidator::Any => NodeBuilder::new_terminal("<any>",Color::BrightBlack),
            AstValidator::Bool => NodeBuilder::new_terminal("bool",TYPE_COLOR),
            AstValidator::SignedSize => NodeBuilder::new_terminal("isize",TYPE_COLOR),
            AstValidator::UnsignedSize => NodeBuilder::new_terminal("usize",TYPE_COLOR),
            AstValidator::Float32 => NodeBuilder::new_terminal("f32",TYPE_COLOR),
            AstValidator::Float64 => NodeBuilder::new_terminal("f64",TYPE_COLOR),
            AstValidator::Complex32 => NodeBuilder::new_terminal("c32",TYPE_COLOR),
            AstValidator::Complex64 => NodeBuilder::new_terminal("c64",TYPE_COLOR),
            AstValidator::Opaque => NodeBuilder::new_terminal("opaque",TYPE_COLOR),
            AstValidator::Void => NodeBuilder::new_terminal("void",TYPE_COLOR),
            AstValidator::CompileTimeFloat => NodeBuilder::new_terminal("comptime_float",TYPE_COLOR),
            AstValidator::CompileTimeComplex => NodeBuilder::new_terminal("comptime_complex",TYPE_COLOR),
            AstValidator::CompileTimeString => NodeBuilder::new_terminal("comptime_string",TYPE_COLOR),
            AstValidator::CompileTimeInteger => NodeBuilder::new_terminal("comptime_integer",TYPE_COLOR),
            AstValidator::Type => NodeBuilder::new_terminal("type",TYPE_COLOR),
            AstValidator::NoReturn => NodeBuilder::new_terminal("noreturn",TYPE_COLOR),
            AstValidator::True => NodeBuilder::new_terminal("true",VALUE_COLOR),
            AstValidator::False => NodeBuilder::new_terminal("false",VALUE_COLOR),
            AstValidator::None => NodeBuilder::new_terminal("none",VALUE_COLOR),
            AstValidator::Underscore => NodeBuilder::new_terminal('_',NAME_COLOR),
            AstValidator::Continue => NodeBuilder::new_terminal("continue",KEYWORD_COLOR),
            AstValidator::EmptyBreak => NodeBuilder::new_terminal("break",KEYWORD_COLOR),
            AstValidator::EmptyReturn => NodeBuilder::new_terminal("return",KEYWORD_COLOR),
            AstValidator::SelfValue => NodeBuilder::new_terminal("self",NAME_COLOR),
            AstValidator::ConstSelfValue => NodeBuilder::new_terminal("~self",NAME_COLOR),
            AstValidator::SelfType => NodeBuilder::new_terminal("Self",TYPE_COLOR),
            AstValidator::ConstReferenceImplicitCapture => NodeBuilder::new_terminal("implicit constant reference capture",OTHER_COLOR),
            AstValidator::ReferenceImplicitCapture => NodeBuilder::new_terminal("implicit reference capture",OTHER_COLOR),
            AstValidator::CopyImplicitCapture => NodeBuilder::new_terminal("implicit copy capture",OTHER_COLOR),
            AstValidator::MatchAll => NodeBuilder::new_terminal("match all",KEYWORD_COLOR),
            AstValidator::NonExhaustive => NodeBuilder::new_terminal("non exhaustive match",KEYWORD_COLOR),
            AstValidator::UnknownSize => NodeBuilder::new_terminal("unknown",VALUE_COLOR),
            AstValidator::InferredSize => NodeBuilder::new_terminal("inferred",VALUE_COLOR),
            AstValidator::SignedIntegerType(size) => NodeBuilder::new_terminal(format!("i{}",size),TYPE_COLOR),
            AstValidator::UnsignedIntegerType(size) => NodeBuilder::new_terminal(format!("u{}",size),TYPE_COLOR),
            AstValidator::StringLiteral(s) => NodeBuilder::new_terminal(s.escape_with_quotes("\""),VALUE_COLOR),
            AstValidator::FloatLiteral(f) => NodeBuilder::new_terminal(format!("{f:?}"),VALUE_COLOR),
            AstValidator::ImaginaryLiteral(i) => NodeBuilder::new_terminal(format!("{i}I"),VALUE_COLOR),
            AstValidator::IntegerLiteral(i) => NodeBuilder::new_terminal(i,VALUE_COLOR),
            AstValidator::NameReference(name) => NodeBuilder::new_terminal(name,NAME_COLOR),
            AstValidator::UnnamedBlock(children) => NodeBuilder::new("unnamed block",KEYWORD_COLOR).add_children(children.iter()).build(),
            AstValidator::TupleLiteral(children) => NodeBuilder::new("tuple literal",OTHER_COLOR).add_children(children.iter()).build(),
            AstValidator::ArrayLiteral(children) => NodeBuilder::new("array literal",OTHER_COLOR).add_children(children.iter()).build(),
            AstValidator::EnumLiteral(name) => NodeBuilder::new("enum literal",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstValidator::BuiltinReference(name) => NodeBuilder::new("builtin reference",OTHER_COLOR).make_inline(name, METHOD_COLOR).build(),
            AstValidator::CopyCapture(name) => NodeBuilder::new("copy capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstValidator::ReferenceCapture(name) => NodeBuilder::new("reference capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstValidator::ConstantReferenceCapture(name) => NodeBuilder::new("constant reference capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstValidator::ObjectLiteral(children) => NodeBuilder::new("object block",OTHER_COLOR).add_children(children.iter()).build(),
            AstValidator::Block(children) => NodeBuilder::new("block",KEYWORD_COLOR).add_children(children.iter()).build(),
            AstValidator::Break(child) => NodeBuilder::new("break",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::Return(child) => NodeBuilder::new("return",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::Yield(child) => NodeBuilder::new("yield",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::Not(child) => NodeBuilder::new("not",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::UnaryMinus(child) => NodeBuilder::new("unary -",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::UnaryPlus(child) => NodeBuilder::new("unary +",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::Dereference(child) => NodeBuilder::new("$",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::AddressOf(child) => NodeBuilder::new("&",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::Concept(child) => NodeBuilder::new("concept",TYPE_COLOR).convert_child(child).build(),
            AstValidator::Loop(child) => NodeBuilder::new("loop",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::FilterTransformation(child) => NodeBuilder::new("?",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::MapTransformation(child) => NodeBuilder::new("|",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::Comptime(child) => NodeBuilder::new("comptime",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::ImplicitResult(child) => NodeBuilder::new("implicit",OTHER_COLOR).convert_child(child).build(),
            AstValidator::Typeof(child) => NodeBuilder::new("typeof",KEYWORD_COLOR).convert_child(child).build(),
            AstValidator::ImplicitArray { constant, subtype } => NodeBuilder::new(if *constant { "implicit constant array type" } else {"implicit array type"},TYPE_COLOR).convert_child(subtype).build(),
            AstValidator::Slice { constant, subtype } => NodeBuilder::new(if *constant { "constant slice type" } else {"slice type"},TYPE_COLOR).convert_child(subtype).build(),
            AstValidator::Reference { constant, subtype } => NodeBuilder::new(if *constant { "constant reference type" } else {"reference type"},TYPE_COLOR).convert_child(subtype).build(),
            AstValidator::TupleCall { functional, args } =>
                NodeBuilder::new("()",KEYWORD_COLOR)
                    .convert_field("value",functional)
                    .list_field("arguments",args.iter())
                    .build(),
            AstValidator::ArrayCall { functional, args } =>
                NodeBuilder::new("[]",KEYWORD_COLOR)
                    .convert_field("value",functional)
                    .list_field("arguments",args.iter())
                    .build(),
            AstValidator::NamedBlock { name, body } =>
                NodeBuilder::new("named block",KEYWORD_COLOR)
                    .make_field("name",name,METHOD_COLOR)
                    .list_field("body",body.iter())
                    .build(),
            AstValidator::NamedBreak { name, value } =>
                NodeBuilder::new("break(...)",KEYWORD_COLOR)
                    .make_field("name",name,METHOD_COLOR)
                    .convert_field("value",value)
                    .build(),
            AstValidator::ObjectCall { functional, args } =>
                NodeBuilder::new("{}",KEYWORD_COLOR)
                    .convert_field("value",functional)
                    .list_field("arguments",args.iter())
                    .build(),
            AstValidator::Match { value, arms } =>
                NodeBuilder::new("match",KEYWORD_COLOR)
                    .convert_field("value",value)
                    .list_field("arms",arms.iter())
                    .build(),
            AstValidator::MatchRange { begin, end } => NodeBuilder::new_binary("match range",KEYWORD_COLOR,begin,end),
            AstValidator::DestructuringMatchArm { matches, store } => {
                let mut builder = NodeBuilder::new("destructuring match",KEYWORD_COLOR);
                match store {
                    None => builder.make_field("store into", "_",NAME_COLOR),
                    Some(store) => builder.convert_field("store into",store),
                }.list_field("matches",matches.iter()).build()
            }
            AstValidator::MatchEnumStructure { enum_identifier, children } => {
                let mut builder = NodeBuilder::new("enum structure match",KEYWORD_COLOR);
                builder.make_field("id",enum_identifier,NAME_COLOR);
                let mut sub_builder = NodeBuilder::new_unnamed();
                for (name, field) in children {
                    sub_builder.convert_field(name,field);
                }
                builder.add_field("fields",sub_builder.build());
                builder.build()
            }
            AstValidator::MatchEnumTuple { enum_identifier, children } =>
                NodeBuilder::new("enum tuple match", KEYWORD_COLOR)
                    .make_field("id",enum_identifier,NAME_COLOR)
                    .list_field("fields",children.iter())
                    .build(),
            AstValidator::MatchValue(value) => value.to_node(),
            AstValidator::MatchConstraint(constraint) =>
                NodeBuilder::new("constrain",KEYWORD_COLOR)
                    .convert_child(constraint)
                    .build(),
            AstValidator::DestructuringMatchStructure(matches) => {
                let mut builder = NodeBuilder::new("structure match",KEYWORD_COLOR);
                for (name, field) in matches {
                    builder.convert_field(name,field);
                }
                builder.build()
            }
            AstValidator::DestructuringMatchTuple(matches) =>
                NodeBuilder::new("tuple match",KEYWORD_COLOR)
                    .add_children(matches.iter())
                    .build(),
            AstValidator::DestructuringMatchArray(matches) =>
                NodeBuilder::new("array match",KEYWORD_COLOR)
                    .add_children(matches.iter())
                    .build(),
            AstValidator::Enum { containing_type, children } => match containing_type {
                None => NodeBuilder::new("enum",TYPE_COLOR).add_children(children.iter()).build(),
                Some(containing_type) => NodeBuilder::new("enum",TYPE_COLOR).convert_field("containing type",containing_type).list_field("values",children.iter()).build(),
            }
            AstValidator::FieldLiteral { name, value } => NodeBuilder::new_unnamed().make_field("name",name,NAME_COLOR).convert_field("value",value).build(),
            AstValidator::Subscription { lhs, rhs } =>  NodeBuilder::new_binary(".",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Multiplication { lhs, rhs } =>  NodeBuilder::new_binary("*",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Division { lhs, rhs } =>  NodeBuilder::new_binary("/",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Modulus { lhs, rhs } =>  NodeBuilder::new_binary("%",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Addition { lhs, rhs } =>  NodeBuilder::new_binary("+",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Subtraction { lhs, rhs } =>  NodeBuilder::new_binary("-",KEYWORD_COLOR,lhs,rhs),
            AstValidator::LeftShift { lhs, rhs } =>  NodeBuilder::new_binary("<<",KEYWORD_COLOR,lhs,rhs),
            AstValidator::RightShift { lhs, rhs } =>  NodeBuilder::new_binary(">>",KEYWORD_COLOR,lhs,rhs),
            AstValidator::LesserThan { lhs, rhs } =>  NodeBuilder::new_binary("<",KEYWORD_COLOR,lhs,rhs),
            AstValidator::GreaterThan { lhs, rhs } =>  NodeBuilder::new_binary(">",KEYWORD_COLOR,lhs,rhs),
            AstValidator::LesserEqual { lhs, rhs } =>  NodeBuilder::new_binary("<=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::GreaterEqual { lhs, rhs } =>  NodeBuilder::new_binary(">=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::EqualTo { lhs, rhs } =>  NodeBuilder::new_binary("==",KEYWORD_COLOR,lhs,rhs),
            AstValidator::NotEqualTo { lhs, rhs } =>  NodeBuilder::new_binary("!=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::And { lhs, rhs } =>  NodeBuilder::new_binary("and",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Or { lhs, rhs } =>  NodeBuilder::new_binary("or",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Xor { lhs, rhs } =>  NodeBuilder::new_binary("xor",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Combine { lhs, rhs } =>  NodeBuilder::new_binary("combine",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Reassign { lhs, rhs } =>  NodeBuilder::new_binary(":=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::Assign { lhs, rhs } =>  NodeBuilder::new_binary("=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::AddAssign { lhs, rhs } =>  NodeBuilder::new_binary("+=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::SubtractAssign { lhs, rhs } =>  NodeBuilder::new_binary("-=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::MultiplyAssign { lhs, rhs } =>  NodeBuilder::new_binary("*=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::DivideAssign { lhs, rhs } =>  NodeBuilder::new_binary("/=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::ModulateAssign { lhs, rhs } =>  NodeBuilder::new_binary("%=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::ShiftLeftAssign { lhs, rhs } =>  NodeBuilder::new_binary("<<=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::ShiftRightAssign { lhs, rhs } =>  NodeBuilder::new_binary(">>=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::AndAssign { lhs, rhs } =>  NodeBuilder::new_binary("and=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::OrAssign { lhs, rhs } =>  NodeBuilder::new_binary("or=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::XorAssign { lhs, rhs } =>  NodeBuilder::new_binary("xor=",KEYWORD_COLOR,lhs,rhs),
            AstValidator::IsType { lhs, rhs } =>  NodeBuilder::new_binary("is",KEYWORD_COLOR,lhs,rhs),
            AstValidator::DynamicCast { lhs, to } =>  NodeBuilder::new_binary("@*",KEYWORD_COLOR,lhs,to),
            AstValidator::Cast { lhs, to } =>  NodeBuilder::new_binary("@",KEYWORD_COLOR,lhs,to),
            AstValidator::Range { begin, end } => NodeBuilder::new_binary("..",KEYWORD_COLOR,begin,end),
            AstValidator::TypeDeclaration { flags, name, generic_arguments, alias } => {
                let mut builder = NodeBuilder::new("type",KEYWORD_COLOR);
                builder.make_field("name", name, TYPE_COLOR);
                builder.make_field("flags",format!("{flags}"),KEYWORD_COLOR);
                if let Some(args) = generic_arguments {
                    builder.list_field("generic arguments",args.iter());
                }
                builder.convert_field("alias", alias);
                builder.build()
            }
            AstValidator::Field { flags, name, field_type } =>
                NodeBuilder::new_unnamed()
                    .make_field("name",name.clone().unwrap_or_else(|| "_".to_string()),NAME_COLOR)
                    .make_field("flags",format!("{flags}"),KEYWORD_COLOR)
                    .convert_field("type",field_type)
                    .build(),
            AstValidator::Argument { name, argument_type } => match name {
                Some(n) =>
                    NodeBuilder::new_unnamed()
                        .make_field("name", n, NAME_COLOR)
                        .convert_field("type", argument_type)
                        .build(),
                None => argument_type.to_node()
            },
            AstValidator::Import { path, name } =>
                NodeBuilder::new("import",KEYWORD_COLOR)
                    .make_field("path",path,VALUE_COLOR)
                    .make_field("name",name,TYPE_COLOR)
                    .build(),
            AstValidator::Structure { is_tuple, interfaces, children } => {
                let mut builder = NodeBuilder::new(if *is_tuple { "tuple" } else { "structure" }, TYPE_COLOR);
                if interfaces.len() == 0 {
                    builder.add_children(children.iter());
                } else {
                    builder.list_field("interfaces", interfaces.iter());
                    builder.list_field("children", children.iter());
                }
                builder.build()
            }
            AstValidator::FunctionPrototype { flags, name, arguments, return_type } => {
                let mut builder = NodeBuilder::new("fn prototype", KEYWORD_COLOR);
                builder.make_field("name",name, METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR).list_field("arguments",arguments.iter());
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.build()
            }
            AstValidator::FunctionImport{ flags, name, arguments, return_type , import_name} => {
                let mut builder = NodeBuilder::new("fn import", KEYWORD_COLOR);
                builder.make_field("name",name, METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR).list_field("arguments",arguments.iter());
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                if let Some(import_name) = import_name {
                    builder.convert_field("library", import_name);
                }
                builder.build()
            }
            AstValidator::Function { flags, name, generic_arguments, arguments, return_type, body } => {
                let mut builder = NodeBuilder::new("fn", KEYWORD_COLOR);
                builder.make_field("name",name, METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR);
                if let Some(args) = generic_arguments {
                    builder.list_field("generic arguments",args.iter());
                }
                builder.list_field("arguments",arguments.iter());
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.convert_field("body", body);
                builder.build()
            }
            AstValidator::Operator { flags, operator, generic_arguments, arguments, return_type, body } => {
                let mut builder = NodeBuilder::new("operator", KEYWORD_COLOR);
                builder.make_field("operator",format!("{operator}"), METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR);
                if let Some(args) = generic_arguments {
                    builder.list_field("generic arguments",args.iter());
                }
                builder.list_field("arguments",arguments.iter());
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.convert_field("body", body);
                builder.build()
            }
            AstValidator::VariableDeclaration { definition, value } => NodeBuilder::new_binary("let",KEYWORD_COLOR,definition,value),
            AstValidator::VariableDefinition { flags, name, variable_type } => {
                let mut builder = NodeBuilder::new("def",KEYWORD_COLOR);
                builder.make_field("name",name,NAME_COLOR).make_field("flags", format!("{flags}"),KEYWORD_COLOR);
                if let Some(variable_type) = variable_type {
                    builder.convert_field("type", variable_type);
                }
                builder.build()
            }
            AstValidator::Closure { arguments, captures, return_type, body } => {
                let mut builder = NodeBuilder::new("closure", KEYWORD_COLOR);
                builder.list_field("arguments",arguments.iter()).list_field("captures",captures.iter());
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.convert_field("body", body);
                builder.build()
            }
            AstValidator::AnonymousFunction { flags, arguments, return_type, body } => {
                let mut builder = NodeBuilder::new("anonymous function", KEYWORD_COLOR);
                builder.make_field("flags",format!("{flags}"),KEYWORD_COLOR).list_field("arguments",arguments.iter());
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.convert_field("body", body);
                builder.build()
            }
            AstValidator::FunctionType { flags, arguments, return_type } =>
                NodeBuilder::new("fn type",TYPE_COLOR)
                    .make_field("flags",format!("{flags}"),KEYWORD_COLOR)
                    .list_field("arguments",arguments.iter())
                    .convert_field("return type",return_type)
                    .build(),
            AstValidator::StructureDestructure(fields) => {
                let mut builder = NodeBuilder::new("structure destructure", OTHER_COLOR);
                for (name, store) in fields {
                    builder.convert_field(name,store);
                }
                builder.build()
            },
            AstValidator::TupleDestructure(fields) => NodeBuilder::new("tuple destructure", OTHER_COLOR).add_children(fields.iter()).build(),
            AstValidator::ArrayDestructure(fields) => NodeBuilder::new("array destructure", OTHER_COLOR).add_children(fields.iter()).build(),
            AstValidator::SliceDestructure(fields) => NodeBuilder::new("slice destructure", OTHER_COLOR).add_children(fields.iter()).build(),
            AstValidator::Destructure { structure, value } => NodeBuilder::new("destructure",KEYWORD_COLOR).convert_field("structure", structure).convert_field("value", value).build(),
            AstValidator::Interface { interfaces, children, dynamic } => {
                let mut builder = NodeBuilder::new(if *dynamic { "dynamic interface" } else { "interface" }, TYPE_COLOR);
                if interfaces.len() == 0 {
                    builder.add_children(children.iter());
                } else {
                    builder.list_field("interfaces", interfaces.iter());
                    builder.list_field("children", children.iter());
                }
                builder.build()
            }
            AstValidator::If { condition, unwrap, body, else_statement } => {
                let mut builder = NodeBuilder::new("if", KEYWORD_COLOR);
                builder.convert_field("condition", condition);
                if let Some(unwrap) = unwrap {
                    builder.convert_field("unwrap", unwrap);
                }
                builder.convert_field("body", body);
                if let Some(else_statement) = else_statement {
                    builder.convert_field("else", else_statement);
                }
                builder.build()
            }
            AstValidator::ComptimeIf { condition, body, else_statement } => {
                let mut builder = NodeBuilder::new("comptime if", KEYWORD_COLOR);
                builder.convert_field("condition", condition);
                builder.convert_field("body", body);
                if let Some(else_statement) = else_statement {
                    builder.convert_field("else", else_statement);
                }
                builder.build()
            }
            AstValidator::While { condition, body, else_statement } => {
                let mut builder = NodeBuilder::new("while", KEYWORD_COLOR);
                builder.convert_field("condition", condition);
                builder.convert_field("body", body);
                if let Some(else_statement) = else_statement {
                    builder.convert_field("else", else_statement);
                }
                builder.build()
            }
            AstValidator::For { capture, index_capture, iterable, transformations, body, else_statement } => {

                let mut builder = NodeBuilder::new("for", KEYWORD_COLOR);
                builder.convert_field("capture", capture);
                if let Some(index_capture) = index_capture {
                    builder.convert_field("index", index_capture);
                }
                builder.convert_field("iterable", iterable);
                if transformations.len() > 0 {
                    builder.list_field("transformations", transformations.iter());
                }
                builder.convert_field("body", body);
                if let Some(else_statement) = else_statement {
                    builder.convert_field("else", else_statement);
                }
                builder.build()
            }
            AstValidator::MatchArm { matches, store, body } => {
                let mut builder = NodeBuilder::new_unnamed();
                if let Some(store) = store {
                    builder.convert_field("capture", store);
                }
                builder.list_field("matches", matches.iter());
                builder.convert_field("body", body);
                builder.build()
            }
            AstValidator::EnumMember { name, tuple, children, value } => if children.len() == 0 && value.is_none() {
                NodeBuilder::new_terminal(name, NAME_COLOR)
            } else {
                let mut builder = NodeBuilder::new_unnamed();
                builder.make_field("name",name,NAME_COLOR);
                if children.len() > 0 {
                    builder.list_field(if *tuple { "tuple" } else {"structure"},children.iter());
                }
                if let Some(value) = value {
                    builder.convert_field("value", value);
                }
                builder.build()
            },
            AstValidator::ArrayType { constant, dimensions, child } =>
                NodeBuilder::new("array type", TYPE_COLOR)
                    .make_field("constant", *constant, VALUE_COLOR)
                    .list_field("dimensions", dimensions.iter())
                    .convert_field("subtype", child)
                    .build(),
            AstValidator::TypeMemberReference { referee, member } =>
                NodeBuilder::new("::",KEYWORD_COLOR)
                    .convert_field("type", referee)
                    .make_field("member", member, NAME_COLOR)
                    .build(),
            AstValidator::GenericInstanceReference { referee, generic_args }  =>
                NodeBuilder::new("::<>",KEYWORD_COLOR)
                    .convert_field("value",referee)
                    .list_field("arguments",generic_args.iter())
                    .build(),
        }
    }
}

pub fn validate_list(list: &crate::ast::NodeList, against: &Vec<Box<AstValidator>>) -> bool {
    if list.len() != against.len() {
        false
    } else {
        for (a, b) in list.iter().zip(against) {
            if !b.validate(a) {
                return false;
            }
        }
        return true;
    }
}

pub fn validate_dict(dict: &crate::ast::NodeDict, against: &HashMap<String, Box<AstValidator>>) -> bool {
    if dict.len() != against.len() {
        false
    } else {
        for (k,v) in dict {
            match against.get(k) {
                None => return false,
                Some(value) => if !value.validate(v) {
                    return false;
                }
            }
        }
        return true;
    }
}

pub fn validate<T: PartialEq>(value: &T, against: &Option<T>) -> bool {
    match against {
        None => true,
        Some(against) => value == against
    }
}

pub fn validate_optional(value: &crate::ast::OptionalNode, against: &Option<Box<AstValidator>>) -> bool {
    match value {
        None => against.is_none(),
        Some(value) => match against {
            None => false,
            Some(against) => against.validate(value)
        }
    }
}

macro_rules! validator {
    ($name:ident) => {
        paste! {
            pub fn [<v_ $name:snake >]  () -> NodePtr {
                Box::new(AstValidator::$name)
            }
        }
    };
    ($name:ident($t:ty)) => {
        paste! {
            pub fn [<v_ $name:snake >]  (v: $t) -> NodePtr {
                Box::new(AstValidator::$name(v))
            }
        }
    };
    ($name:ident{$($field:ident:$t:ty),*$(,)?}) => {
        paste! {
            pub fn [<v_ $name:snake >] ($($field: $t),*) -> NodePtr {
                Box::new(AstValidator::$name{
                    $(
                        $field
                    ),*
                })
            }
        }
    };
}
macro_rules! validator_set {
    ($($name: ident),*$(,)?) => {
        $(
            validator!($name);
        )*
    };
    ($($name:ident($t:ty)),*$(,)?) => {
        $(
            validator!($name($t));
        )*
    };
}


validator_set!(
    Any,
    Bool,
    SignedSize,
    UnsignedSize,
    Float32,
    Float64,
    Complex32,
    Complex64,
    Opaque,
    Void,
    CompileTimeFloat,
    CompileTimeComplex,
    CompileTimeString,
    CompileTimeInteger,
    Type,
    NoReturn,
    True,
    False,
    None,
    Underscore,
    Continue,
    EmptyBreak,
    EmptyReturn,
    SelfValue,
    ConstSelfValue,
    SelfType,
    ConstReferenceImplicitCapture,
    ReferenceImplicitCapture,
    CopyImplicitCapture,
    MatchAll,
    NonExhaustive,
    UnknownSize,
    InferredSize
);
validator_set!(
    SignedIntegerType(u64),
    UnsignedIntegerType(u64),
    StringLiteral(String),
    FloatLiteral(f64),
    ImaginaryLiteral(f64),
    IntegerLiteral(BigInt),
    NameReference(String),
    UnnamedBlock(NodeList),
    TupleLiteral(NodeList),
    ArrayLiteral(NodeList),
    EnumLiteral(String),
    BuiltinReference(String),
    CopyCapture(String),
    ReferenceCapture(String),
    ConstantReferenceCapture(String),
    ObjectLiteral(NodeList),
    Block(NodeList),
    Break(NodePtr),
    Return(NodePtr),
    Yield(NodePtr),
    Not(NodePtr),
    UnaryMinus(NodePtr),
    UnaryPlus(NodePtr),
    Dereference(NodePtr),
    AddressOf(NodePtr),
    Concept(NodePtr),
    Loop(NodePtr),
    FilterTransformation(NodePtr),
    MapTransformation(NodePtr),
    Comptime(NodePtr),
    ImplicitResult(NodePtr),
    MatchValue(NodePtr),
    MatchConstraint(NodePtr),
    DestructuringMatchStructure(NodeDict),
    DestructuringMatchTuple(NodeList),
    DestructuringMatchArray(NodeList),
    StructureDestructure(NodeDict),
    TupleDestructure(NodeList),
    ArrayDestructure(NodeList),
    SliceDestructure(NodeList),
    Typeof(NodePtr)
);


validator!(ImplicitArray{
        constant: bool,
        subtype: NodePtr
});

validator!(Slice {
        constant: bool,
        subtype: NodePtr
});
validator!(Reference{
    constant: bool,
    subtype: NodePtr
});
validator!(TupleCall {
        functional: NodePtr,
        args: NodeList
    });
validator!(ArrayCall {
        functional: NodePtr,
        args: NodeList
    });
validator!(NamedBlock {
        name: String,
        body: NodeList
    });
validator!(NamedBreak {
        name: String,
        value: NodePtr
    });
validator!(ObjectCall {
        functional: NodePtr,
        args: NodeList
    });
validator!(Match {
        value: NodePtr,
        arms: NodeList
    });
validator!(MatchRange {
        begin: NodePtr,
        end: NodePtr
    });

validator!(DestructuringMatchArm  {
    matches: NodeList,
    store: OptionalNode,
});
validator!(MatchEnumStructure  {
    enum_identifier: String,
    children: NodeDict,
});
validator!(MatchEnumTuple  {
    enum_identifier: String,
    children: NodeList,
});
validator!(Enum {
    containing_type: OptionalNode,
    children: NodeList,
});
validator!(FieldLiteral {
    name: String,
    value: NodePtr,
});
validator!(Subscription {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Multiplication {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Division {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Modulus {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Addition {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Subtraction {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(LeftShift {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(RightShift {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(LesserThan {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(GreaterThan {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(LesserEqual {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(GreaterEqual {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(EqualTo {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(NotEqualTo {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(And {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Or {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Xor {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Combine {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Reassign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(Assign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(AddAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(SubtractAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(MultiplyAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(DivideAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(ModulateAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(ShiftLeftAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(ShiftRightAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(AndAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(OrAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(XorAssign {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(IsType {
    lhs: NodePtr,
    rhs: NodePtr,
});
validator!(DynamicCast {
    lhs: NodePtr,
    to: NodePtr,
});
validator!(Cast {
    lhs: NodePtr,
    to: NodePtr,
});
validator!(Range {
    begin: NodePtr,
    end: NodePtr,
});
validator!(TypeDeclaration {
    flags: DeclarationFlags,
    name: String,
    generic_arguments: Option<NodeList>,
    alias: NodePtr,
});
validator!(Field {
    flags: DeclarationFlags,
    name: Option<String>,
    field_type: NodePtr,
});
validator!(Argument {
    name: Option<String>,
    argument_type: NodePtr,
});
validator!(Import {
    path: String,
    name: String,
});
validator!(Structure {
    is_tuple: bool,
    interfaces: NodeList,
    children: NodeList,
});
validator!(FunctionPrototype {
    flags: DeclarationFlags,
    name: String,
    arguments: NodeList,
    return_type: OptionalNode,
});
validator!(FunctionImport {
    flags: DeclarationFlags,
    name: String,
    arguments: NodeList,
    return_type: OptionalNode,
    import_name: OptionalNode,
});
validator!(Function {
    flags: DeclarationFlags,
    name: String,
    generic_arguments: Option<NodeList>,
    arguments: NodeList,
    return_type: OptionalNode,
    body: NodePtr,
});
validator!(Operator {
    flags: DeclarationFlags,
    operator: Operator,
    generic_arguments: Option<NodeList>,
    arguments: NodeList,
    return_type: OptionalNode,
    body: NodePtr,
});
validator!(VariableDeclaration {
    definition: NodePtr,
    value: NodePtr,
});
validator!(VariableDefinition {
    flags: DeclarationFlags,
    name: String,
    variable_type: OptionalNode,
});
validator!(Closure {
    arguments: NodeList,
    captures: NodeList,
    return_type: OptionalNode,
    body: NodePtr,
});
validator!(AnonymousFunction {
    flags: DeclarationFlags,
    arguments: NodeList,
    return_type: OptionalNode,
    body: NodePtr,
});
validator!(FunctionType {
    flags: DeclarationFlags,
    arguments: NodeList,
    return_type: NodePtr,
});


validator!(Destructure {
    structure: NodePtr,
    value: NodePtr,
});
validator!(Interface {
    interfaces: NodeList,
    children: NodeList,
    dynamic: bool,
});
validator!(If {
    condition: NodePtr,
    unwrap: OptionalNode,
    body: NodePtr,
    else_statement: OptionalNode,
});
validator!(ComptimeIf {
    condition: NodePtr,
    body: NodePtr,
    else_statement: OptionalNode,
});
validator!(While {
    condition: NodePtr,
    body: NodePtr,
    else_statement: OptionalNode,
});
validator!(For {
    capture: NodePtr,
    index_capture: OptionalNode,
    iterable: NodePtr,
    transformations: NodeList,
    body: NodePtr,
    else_statement: OptionalNode,
});
validator!(MatchArm {
    matches: NodeList,
    store: OptionalNode,
    body: NodePtr,
});
validator!(EnumMember {
    name: String,
    tuple: bool,
    children: NodeList,
    value: OptionalNode,
});
validator!(ArrayType {
    constant: bool,
    dimensions: NodeList,
    child: NodePtr,
});
validator!(TypeMemberReference {
    referee: NodePtr,
    member: String
});
validator!(GenericInstanceReference {
    referee: NodePtr,
    generic_args: NodeList
});

pub fn v_program(children: NodeList) -> NodePtr {
    v_structure(false, vec![], children)
}

pub fn v_single(child: NodePtr) -> NodePtr {
    v_program(vec![child])
}

pub fn v_name<T: ToString>(name: T) -> NodePtr {
    v_name_reference(name.to_string())
}

pub fn v_string<T: ToString>(name: T) -> NodePtr {
    v_string_literal(name.to_string())
}

#[macro_export]
macro_rules! v_map {
     ($($key:expr => $value:expr),*$(,)?) => {
         {
            let mut temp_map = HashMap::new();
            $(
               temp_map.insert($key.to_string(), $value);
            )*
            temp_map
         }
     };
 }