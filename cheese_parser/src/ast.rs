use std::collections::HashMap;
use std::fmt::{Display, format, Formatter, Pointer};
use std::fs::canonicalize;
use ariadne::{Color, Fmt};
use bitflags::bitflags;
use num_bigint::BigInt;
use cheese_diagnostics::ErrorCode;
use cheese_diagnostics::locating::FileSpan;
use cheese_utilities::strings::Escapable;
use cheese_utilities::trees::{DisplayableTree, DisplayNode, NodeBuilder};

pub type NodePtr = Box<AstNode>;
pub type NodeList = Vec<NodePtr>;
pub type NodeDict = HashMap<String, NodePtr>;
pub type OptionalNode = Option<NodePtr>;
pub type PipesList = Vec<(usize,bool)>;
bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct DeclarationFlags: u8 {
        const inline   = 0b0000_0001;
        const external = 0b0000_0010;
        const export   = 0b0000_0100;
        const comptime = 0b0000_1000;
        const public   = 0b0001_0000;
        const private  = 0b0010_0000;
        const mutable  = 0b0100_0000;
        const entry    = 0b1000_0000;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    TupleCall,
    ArrayCall,
    ObjectCall,
    Subscript,
    UnaryPlus,
    Add,
    UnaryMinus,
    Subtract,
    Dereference,
    Not,
    Multiply,
    Divide,
    Modulate,
    ShiftLeft,
    ShiftRight,
    Lesser,
    Greater,
    LesserEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
    Xor,
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModulateAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    Unknown
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Operator::TupleCall => "tuple_call",
            Operator::ArrayCall => "array_call",
            Operator::ObjectCall => "object_call",
            Operator::Subscript => "subscript",
            Operator::UnaryPlus => "unary_plus",
            Operator::Add => "add",
            Operator::UnaryMinus => "unary_minus",
            Operator::Subtract => "subtract",
            Operator::Dereference => "dereference",
            Operator::Not => "not",
            Operator::Multiply => "multiply",
            Operator::Divide => "divide",
            Operator::Modulate => "modulate",
            Operator::ShiftLeft => "shift_left",
            Operator::ShiftRight => "shift_right",
            Operator::Lesser => "lesser",
            Operator::Greater => "greater",
            Operator::LesserEqual => "lesser_equal",
            Operator::GreaterEqual => "greater_equal",
            Operator::Equal => "equal",
            Operator::NotEqual => "not_equal",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::Xor => "xor",
            Operator::Assign => "assign",
            Operator::AddAssign => "add_assign",
            Operator::SubtractAssign => "subtract_assign",
            Operator::MultiplyAssign => "multiply_assign",
            Operator::DivideAssign => "divide_assign",
            Operator::ModulateAssign => "modulate_assign",
            Operator::ShiftLeftAssign => "shift_left_assign",
            Operator::ShiftRightAssign => "shift_right_assign",
            Operator::AndAssign => "and_assign",
            Operator::OrAssign => "or_assign",
            Operator::XorAssign => "xor_assign",
            Operator::Unknown => "<unknown>"
        })
    }
}

impl Display for DeclarationFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.0 == 0 {
            return write!(f, "none")
        }
        let mut prefix = "";
        if self.contains(Self::inline) {
            write!(f, "{}inline", prefix)?;
            prefix = " ";
        }
        if self.contains(Self::external) {
            write!(f, "{}external", prefix)?;
            prefix = " ";
        }
        if self.contains(Self::export) {
            write!(f, "{}export", prefix)?;
            prefix = " ";
        }
        if self.contains(Self::comptime) {
            write!(f, "{}comptime", prefix, )?;
            prefix = " ";
        }
        if self.contains(Self::public) {
            write!(f, "{}public", prefix)?;
            prefix = " ";
        }
        if self.contains(Self::private) {
            write!(f, "{}private", prefix)?;
            prefix = " ";
        }
        if self.contains(Self::mutable) {
            write!(f, "{}mutable", prefix)?;
            prefix = " ";
        }
        if self.contains(Self::entry) {
            write!(f, "{}entry", prefix)?;
            prefix = " ";
        }
        Ok(())
    }
}
pub struct AstNode {
    pub span: FileSpan,
    pub data: AstNodeData,
}
impl AstNode {
    pub fn new(span: FileSpan, data: AstNodeData) -> Box<Self> {
        Box::new(AstNode{span,data})
    }
}
impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let tree = self.to_node();
        std::fmt::Display::fmt(&tree, f)
    }
}
#[derive(strum_macros::Display)]
pub enum AstNodeData {
    // Let's make all the node types
    // Bool := bool
    Bool,
    // SignedSize := isize
    SignedSize,
    // UnsignedSize := usize
    UnsignedSize,
    // Float32 :=  f32
    Float32,
    // Float64 := f64
    Float64,
    // Complex32 := c32
    Complex32,
    // Complex64 := c64
    Complex64,
    // Opaque := opaque
    Opaque,
    // Void := void
    Void,
    // CompileTimeFloat := comptime_float
    CompileTimeFloat,
    // CompileTimeComplex := comptime_complex
    CompileTimeComplex,
    // CompileTimeString := comptime_string
    CompileTimeString,
    // CompileTimeInteger := comptime_integer
    CompileTimeInteger,
    // Type := type
    Type,
    // NoReturn := noreturn
    NoReturn,
    // True := true
    True,
    // False := false
    False,
    // None := none
    None,
    // Underscore := _
    Underscore,
    // Continue := continue
    Continue,
    // EmptyBreak := break;
    EmptyBreak,
    // EmptyReturn := return;
    EmptyReturn,
    // SelfValue := self
    SelfValue,
    // ConstSelfValue := ~self
    ConstSelfValue,
    // SelfType := Self
    SelfType,
    // ConstReferenceImplicitCapture := *~
    ConstReferenceImplicitCapture,
    // ReferenceImplicitCapture := *
    ReferenceImplicitCapture,
    // CopyImplicitCapture := =
    CopyImplicitCapture,
    // MatchAll := _
    MatchAll,
    // NonExhaustive := _
    NonExhaustive,
    // UnknownSize := ?
    UnknownSize,
    // InferredSize := /* nothing */
    InferredSize,

    // SignedIntegerType := i[size]
    SignedIntegerType(u64),
    // UnsignedIntegerType := u[size]
    UnsignedIntegerType(u64),
    // String Literal := "..."
    StringLiteral(String),
    // Float Literal := [float]
    FloatLiteral(f64),
    // Imaginary Literal := [float]I
    ImaginaryLiteral(f64),
    // Integer Literal := [int]
    IntegerLiteral(BigInt),
    // NameReference := [identifier]
    NameReference(String),
    // UnnamedBlock := { ... }
    UnnamedBlock(NodeList),
    // TupleLiteral := .( ... )
    TupleLiteral(NodeList),
    // ArrayLiteral := .[ ... ]
    ArrayLiteral(NodeList),
    // EnumLiteral := . [identifier]
    EnumLiteral(String),
    // BuiltinReference := $[identifier]
    BuiltinReference(String),
    // CopyCapture := =[identifier]
    CopyCapture(String),
    // ReferenceCapture := *[identifier]
    ReferenceCapture(String),
    // ConstantReferenceCapture := *~[identifier]
    ConstantReferenceCapture(String),
    // ObjectLiteral := .{ ... }
    ObjectLiteral(NodeList),
    // Block := { ... }
    Block(NodeList),
    // Break := break [value]
    Break(NodePtr),
    // Return := return [value]
    Return(NodePtr),
    // Yield  := yield [value]
    Yield(NodePtr),
    // Not := not [value]
    Not(NodePtr),
    // UnaryMinus := -[value]
    UnaryMinus(NodePtr),
    // UnaryPlus := +[value]
    UnaryPlus(NodePtr),
    // Dereference := *[value]
    Dereference(NodePtr),
    // AddressOf := &[value]
    AddressOf(NodePtr),
    // Concept := concept [statement]
    Concept(NodePtr),
    // Loop := loop [expression]
    Loop(NodePtr),
    // FilterTransformation := ? [expression]
    FilterTransformation(NodePtr),
    // MapTransformation := : [expression]
    MapTransformation(NodePtr),
    // Comptime := comptime [expression]
    Comptime(NodePtr),
    // ImplicitResult := statement at end of block without semicolon
    ImplicitResult(NodePtr),
    // ImplicitArray := [](~)[subtype]
    Typeof(NodePtr),
    // Typeof := typeof type
    ImplicitArray {
        constant: bool,
        subtype: NodePtr,
    },
    // Slice := <>(~)[subtype]
    Slice {
        constant: bool,
        subtype: NodePtr,
    },
    // *(~)[subtype]
    Reference {
        constant: bool,
        subtype: NodePtr,
    },

    // TupleCall := [lhs](...)
    TupleCall {
        functional: NodePtr,
        args: NodeList,
    },
    // ArrayCall := [lhs][...]
    ArrayCall {
        functional: NodePtr,
        args: NodeList,
    },
    // NamedBlock := :(name) { ... }
    NamedBlock {
        name: String,
        body: NodeList,
    },
    // NamedBreak := yield(name) [value]
    NamedBreak {
        name: String,
        value: NodePtr,
    },
    // ObjectCall := [lhs]{ ... }
    ObjectCall {
        functional: NodePtr,
        args: NodeList,
    },
    // ErrorNode := ERROR
    ErrorNode {
        error_code: ErrorCode,
        message: String,
    },
    // Match := match (value) { ... }
    Match {
        value: NodePtr,
        arms: NodeList,
    },
    // MatchRange := [begin] .. [end]
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
    // MatchValue := expr
    MatchValue(NodePtr),
    // MatchConstraint := constrain [functional]
    MatchConstraint(NodePtr),
    // DestructuringMatchStructure := .{ ... }
    DestructuringMatchStructure(NodeDict),
    // DestructuringMatchTuple := .( ... )
    DestructuringMatchTuple(NodeList),
    // DestructuringMatchArray := .[ ... ]
    DestructuringMatchArray(NodeList),
    Enum {
        containing_type: OptionalNode,
        children: NodeList,
    },
    // FieldLiteral := value : expr
    FieldLiteral {
        name: String,
        value: NodePtr,
    },
    // Binary operators :3
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
    // TypeDeclaration := type [name](<...>) is [type]
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
        return_type: OptionalNode, // If there is no return type it is assumed void
        body: NodePtr,
    },

    Operator {
        flags: DeclarationFlags,
        operator: Operator,
        generic_arguments: Option<NodeList>,
        arguments: NodeList,
        return_type: OptionalNode, // If there is no return type it is deduced :3
        body: NodePtr,
    },

    // Not going to implement generators in the C version of this

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
        return_type: OptionalNode, // If there is no return type it is assumed to be void
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

impl DisplayableTree for AstNode {
    fn to_node(&self) -> Box<DisplayNode> {
        self.data.to_node()
    }
}

const TYPE_COLOR: Color = Color::Red;
const VALUE_COLOR: Color = Color::Magenta;
const NAME_COLOR: Color = Color::Cyan;
const KEYWORD_COLOR: Color = Color::Blue;
const METHOD_COLOR: Color = Color::BrightYellow;
const OTHER_COLOR: Color = Color::Green;

impl DisplayableTree for AstNodeData {
    fn to_node(&self) -> Box<DisplayNode> {
        match self {
            AstNodeData::Bool => NodeBuilder::new_terminal("bool",TYPE_COLOR),
            AstNodeData::SignedSize => NodeBuilder::new_terminal("isize",TYPE_COLOR),
            AstNodeData::UnsignedSize => NodeBuilder::new_terminal("usize",TYPE_COLOR),
            AstNodeData::Float32 => NodeBuilder::new_terminal("f32",TYPE_COLOR),
            AstNodeData::Float64 => NodeBuilder::new_terminal("f64",TYPE_COLOR),
            AstNodeData::Complex32 => NodeBuilder::new_terminal("c32",TYPE_COLOR),
            AstNodeData::Complex64 => NodeBuilder::new_terminal("c64",TYPE_COLOR),
            AstNodeData::Opaque => NodeBuilder::new_terminal("opaque",TYPE_COLOR),
            AstNodeData::Void => NodeBuilder::new_terminal("void",TYPE_COLOR),
            AstNodeData::CompileTimeFloat => NodeBuilder::new_terminal("comptime_float",TYPE_COLOR),
            AstNodeData::CompileTimeComplex => NodeBuilder::new_terminal("comptime_complex",TYPE_COLOR),
            AstNodeData::CompileTimeString => NodeBuilder::new_terminal("comptime_string",TYPE_COLOR),
            AstNodeData::CompileTimeInteger => NodeBuilder::new_terminal("comptime_integer",TYPE_COLOR),
            AstNodeData::Type => NodeBuilder::new_terminal("type",TYPE_COLOR),
            AstNodeData::NoReturn => NodeBuilder::new_terminal("noreturn",TYPE_COLOR),
            AstNodeData::True => NodeBuilder::new_terminal("true",VALUE_COLOR),
            AstNodeData::False => NodeBuilder::new_terminal("false",VALUE_COLOR),
            AstNodeData::None => NodeBuilder::new_terminal("none",VALUE_COLOR),
            AstNodeData::Underscore => NodeBuilder::new_terminal('_',NAME_COLOR),
            AstNodeData::Continue => NodeBuilder::new_terminal("continue",KEYWORD_COLOR),
            AstNodeData::EmptyBreak => NodeBuilder::new_terminal("break",KEYWORD_COLOR),
            AstNodeData::EmptyReturn => NodeBuilder::new_terminal("return",KEYWORD_COLOR),
            AstNodeData::SelfValue => NodeBuilder::new_terminal("self",NAME_COLOR),
            AstNodeData::ConstSelfValue => NodeBuilder::new_terminal("~self",NAME_COLOR),
            AstNodeData::SelfType => NodeBuilder::new_terminal("Self",TYPE_COLOR),
            AstNodeData::ConstReferenceImplicitCapture => NodeBuilder::new_terminal("implicit constant reference capture",OTHER_COLOR),
            AstNodeData::ReferenceImplicitCapture => NodeBuilder::new_terminal("implicit reference capture",OTHER_COLOR),
            AstNodeData::CopyImplicitCapture => NodeBuilder::new_terminal("implicit copy capture",OTHER_COLOR),
            AstNodeData::MatchAll => NodeBuilder::new_terminal("match all",KEYWORD_COLOR),
            AstNodeData::NonExhaustive => NodeBuilder::new_terminal("non exhaustive match",KEYWORD_COLOR),
            AstNodeData::UnknownSize => NodeBuilder::new_terminal("unknown",VALUE_COLOR),
            AstNodeData::InferredSize => NodeBuilder::new_terminal("inferred",VALUE_COLOR),
            AstNodeData::SignedIntegerType(size) => NodeBuilder::new_terminal(format!("i{}",size),TYPE_COLOR),
            AstNodeData::UnsignedIntegerType(size) => NodeBuilder::new_terminal(format!("u{}",size),TYPE_COLOR),
            AstNodeData::StringLiteral(s) => NodeBuilder::new_terminal(s.escape_with_quotes("\""),VALUE_COLOR),
            AstNodeData::FloatLiteral(f) => NodeBuilder::new_terminal(format!("{f:?}"),VALUE_COLOR),
            AstNodeData::ImaginaryLiteral(i) => NodeBuilder::new_terminal(format!("{i}I"),VALUE_COLOR),
            AstNodeData::IntegerLiteral(i) => NodeBuilder::new_terminal(i,VALUE_COLOR),
            AstNodeData::NameReference(name) => NodeBuilder::new_terminal(name,NAME_COLOR),
            AstNodeData::UnnamedBlock(children) => NodeBuilder::new("unnamed block",KEYWORD_COLOR).add_children(children.iter()).build(),
            AstNodeData::TupleLiteral(children) => NodeBuilder::new("tuple literal",OTHER_COLOR).add_children(children.iter()).build(),
            AstNodeData::ArrayLiteral(children) => NodeBuilder::new("array literal",OTHER_COLOR).add_children(children.iter()).build(),
            AstNodeData::EnumLiteral(name) => NodeBuilder::new("enum literal",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::BuiltinReference(name) => NodeBuilder::new("builtin reference",OTHER_COLOR).make_inline(name, METHOD_COLOR).build(),
            AstNodeData::CopyCapture(name) => NodeBuilder::new("copy capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::ReferenceCapture(name) => NodeBuilder::new("reference capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::ConstantReferenceCapture(name) => NodeBuilder::new("constant reference capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::ObjectLiteral(children) => NodeBuilder::new("object block",OTHER_COLOR).add_children(children.iter()).build(),
            AstNodeData::Block(children) => NodeBuilder::new("block",KEYWORD_COLOR).add_children(children.iter()).build(),
            AstNodeData::Break(child) => NodeBuilder::new("break",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::Return(child) => NodeBuilder::new("return",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::Yield(child) => NodeBuilder::new("yield",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::Not(child) => NodeBuilder::new("not",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::UnaryMinus(child) => NodeBuilder::new("unary -",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::UnaryPlus(child) => NodeBuilder::new("unary +",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::Dereference(child) => NodeBuilder::new("$",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::AddressOf(child) => NodeBuilder::new("&",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::Concept(child) => NodeBuilder::new("concept",TYPE_COLOR).convert_child(child).build(),
            AstNodeData::Loop(child) => NodeBuilder::new("loop",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::FilterTransformation(child) => NodeBuilder::new("?",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::MapTransformation(child) => NodeBuilder::new("|",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::Comptime(child) => NodeBuilder::new("comptime",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::ImplicitResult(child) => NodeBuilder::new("implicit",OTHER_COLOR).convert_child(child).build(),
            AstNodeData::Typeof(child) => NodeBuilder::new("typeof",KEYWORD_COLOR).convert_child(child).build(),
            AstNodeData::ImplicitArray { constant, subtype } => NodeBuilder::new(if *constant { "implicit constant array type" } else {"implicit array type"},TYPE_COLOR).convert_child(subtype).build(),
            AstNodeData::Slice { constant, subtype } => NodeBuilder::new(if *constant { "constant slice type" } else {"slice type"},TYPE_COLOR).convert_child(subtype).build(),
            AstNodeData::Reference { constant, subtype } => NodeBuilder::new(if *constant { "constant reference type" } else {"reference type"},TYPE_COLOR).convert_child(subtype).build(),
            AstNodeData::TupleCall { functional, args } =>
                NodeBuilder::new("()",KEYWORD_COLOR)
                    .convert_field("value",functional)
                    .list_field("arguments",args.iter())
                    .build(),
            AstNodeData::ArrayCall { functional, args } =>
                NodeBuilder::new("[]",KEYWORD_COLOR)
                    .convert_field("value",functional)
                    .list_field("arguments",args.iter())
                    .build(),
            AstNodeData::NamedBlock { name, body } =>
                NodeBuilder::new("named block",KEYWORD_COLOR)
                    .make_field("name",name,METHOD_COLOR)
                    .list_field("body",body.iter())
                    .build(),
            AstNodeData::NamedBreak { name, value } =>
                NodeBuilder::new("break(...)",KEYWORD_COLOR)
                    .make_field("name",name,METHOD_COLOR)
                    .convert_field("value",value)
                    .build(),
            AstNodeData::ObjectCall { functional, args } =>
                NodeBuilder::new("{}",KEYWORD_COLOR)
                    .convert_field("value",functional)
                    .list_field("arguments",args.iter())
                    .build(),
            AstNodeData::ErrorNode { message, .. } => NodeBuilder::new_terminal(message,Color::BrightRed),
            AstNodeData::Match { value, arms } =>
                NodeBuilder::new("match",KEYWORD_COLOR)
                    .convert_field("value",value)
                    .list_field("arms",arms.iter())
                    .build(),
            AstNodeData::MatchRange { begin, end } => NodeBuilder::new_binary("match range",KEYWORD_COLOR,begin,end),
            AstNodeData::DestructuringMatchArm { matches, store } => {
                let mut builder = NodeBuilder::new("destructuring match",KEYWORD_COLOR);
                match store {
                    None => builder.make_field("store into", "_",NAME_COLOR),
                    Some(store) => builder.convert_field("store into",store),
                }.list_field("matches",matches.iter()).build()
            }
            AstNodeData::MatchEnumStructure { enum_identifier, children } => {
                let mut builder = NodeBuilder::new("enum structure match",KEYWORD_COLOR);
                builder.make_field("id",enum_identifier,NAME_COLOR);
                let mut sub_builder = NodeBuilder::new_unnamed();
                for (name, field) in children {
                    sub_builder.convert_field(name,field);
                }
                builder.add_field("fields",sub_builder.build());
                builder.build()
            }
            AstNodeData::MatchEnumTuple { enum_identifier, children } =>
                NodeBuilder::new("enum tuple match", KEYWORD_COLOR)
                    .make_field("id",enum_identifier,NAME_COLOR)
                    .list_field("fields",children.iter())
                    .build(),
            AstNodeData::MatchValue(value) => value.to_node(),
            AstNodeData::MatchConstraint(constraint) =>
                NodeBuilder::new("constrain",KEYWORD_COLOR)
                    .convert_child(constraint)
                    .build(),
            AstNodeData::DestructuringMatchStructure(matches) => {
                let mut builder = NodeBuilder::new("structure match",KEYWORD_COLOR);
                for (name, field) in matches {
                    builder.convert_field(name,field);
                }
                builder.build()
            }
            AstNodeData::DestructuringMatchTuple(matches) =>
                NodeBuilder::new("tuple match",KEYWORD_COLOR)
                    .add_children(matches.iter())
                    .build(),
            AstNodeData::DestructuringMatchArray(matches) =>
                NodeBuilder::new("array match",KEYWORD_COLOR)
                    .add_children(matches.iter())
                    .build(),
            AstNodeData::Enum { containing_type, children } => match containing_type {
                None => NodeBuilder::new("enum",TYPE_COLOR).add_children(children.iter()).build(),
                Some(containing_type) => NodeBuilder::new("enum",TYPE_COLOR).convert_field("containing type",containing_type).list_field("values",children.iter()).build(),
            }
            AstNodeData::FieldLiteral { name, value } => NodeBuilder::new_unnamed().make_field("name",name,NAME_COLOR).convert_field("value",value).build(),
            AstNodeData::Subscription { lhs, rhs } =>  NodeBuilder::new_binary(".",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Multiplication { lhs, rhs } =>  NodeBuilder::new_binary("*",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Division { lhs, rhs } =>  NodeBuilder::new_binary("/",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Modulus { lhs, rhs } =>  NodeBuilder::new_binary("%",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Addition { lhs, rhs } =>  NodeBuilder::new_binary("+",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Subtraction { lhs, rhs } =>  NodeBuilder::new_binary("-",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::LeftShift { lhs, rhs } =>  NodeBuilder::new_binary("<<",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::RightShift { lhs, rhs } =>  NodeBuilder::new_binary(">>",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::LesserThan { lhs, rhs } =>  NodeBuilder::new_binary("<",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::GreaterThan { lhs, rhs } =>  NodeBuilder::new_binary(">",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::LesserEqual { lhs, rhs } =>  NodeBuilder::new_binary("<=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::GreaterEqual { lhs, rhs } =>  NodeBuilder::new_binary(">=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::EqualTo { lhs, rhs } =>  NodeBuilder::new_binary("==",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::NotEqualTo { lhs, rhs } =>  NodeBuilder::new_binary("!=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::And { lhs, rhs } =>  NodeBuilder::new_binary("and",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Or { lhs, rhs } =>  NodeBuilder::new_binary("or",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Xor { lhs, rhs } =>  NodeBuilder::new_binary("xor",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Combine { lhs, rhs } =>  NodeBuilder::new_binary("combine",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Reassign { lhs, rhs } =>  NodeBuilder::new_binary(":=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::Assign { lhs, rhs } =>  NodeBuilder::new_binary("=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::AddAssign { lhs, rhs } =>  NodeBuilder::new_binary("+=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::SubtractAssign { lhs, rhs } =>  NodeBuilder::new_binary("-=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::MultiplyAssign { lhs, rhs } =>  NodeBuilder::new_binary("*=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::DivideAssign { lhs, rhs } =>  NodeBuilder::new_binary("/=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::ModulateAssign { lhs, rhs } =>  NodeBuilder::new_binary("%=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::ShiftLeftAssign { lhs, rhs } =>  NodeBuilder::new_binary("<<=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::ShiftRightAssign { lhs, rhs } =>  NodeBuilder::new_binary(">>=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::AndAssign { lhs, rhs } =>  NodeBuilder::new_binary("and=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::OrAssign { lhs, rhs } =>  NodeBuilder::new_binary("or=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::XorAssign { lhs, rhs } =>  NodeBuilder::new_binary("xor=",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::IsType { lhs, rhs } =>  NodeBuilder::new_binary("is",KEYWORD_COLOR,lhs,rhs),
            AstNodeData::DynamicCast { lhs, to } =>  NodeBuilder::new_binary("@*",KEYWORD_COLOR,lhs,to),
            AstNodeData::Cast { lhs, to } =>  NodeBuilder::new_binary("@",KEYWORD_COLOR,lhs,to),
            AstNodeData::Range { begin, end } => NodeBuilder::new_binary("..",KEYWORD_COLOR,begin,end),
            AstNodeData::TypeDeclaration { flags, name, generic_arguments, alias } => {
                let mut builder = NodeBuilder::new("type",KEYWORD_COLOR);
                builder.make_field("name", name, TYPE_COLOR);
                builder.make_field("flags",format!("{flags}"),KEYWORD_COLOR);
                if let Some(args) = generic_arguments {
                    builder.list_field("generic arguments",args.iter());
                }
                builder.convert_field("alias", alias);
                builder.build()
            }
            AstNodeData::Field { flags, name, field_type } =>
                NodeBuilder::new_unnamed()
                    .make_field("name",name.clone().unwrap_or_else(|| "_".to_string()),NAME_COLOR)
                    .make_field("flags",format!("{flags}"),KEYWORD_COLOR)
                    .convert_field("type",field_type)
                    .build(),
            AstNodeData::Argument { name, argument_type } => match name {
                Some(n) =>
                    NodeBuilder::new_unnamed()
                        .make_field("name", n, NAME_COLOR)
                        .convert_field("type", argument_type)
                        .build(),
                None => argument_type.to_node()
            },
            AstNodeData::Import { path, name } =>
                NodeBuilder::new("import",KEYWORD_COLOR)
                    .make_field("path",path,VALUE_COLOR)
                    .make_field("name",name,TYPE_COLOR)
                    .build(),
            AstNodeData::Structure { is_tuple, interfaces, children } => {
                let mut builder = NodeBuilder::new(if *is_tuple { "tuple" } else { "structure" }, TYPE_COLOR);
                if interfaces.len() == 0 {
                    builder.add_children(children.iter());
                } else {
                    builder.list_field("interfaces", interfaces.iter());
                    builder.list_field("children", children.iter());
                }
                builder.build()
            }
            AstNodeData::FunctionPrototype { flags, name, arguments, return_type } => {
                let mut builder = NodeBuilder::new("fn prototype", KEYWORD_COLOR);
                builder.make_field("name",name, METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR).list_field("arguments",arguments.iter());
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.build()
            }
            AstNodeData::FunctionImport{ flags, name, arguments, return_type , import_name} => {
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
            AstNodeData::Function { flags, name, generic_arguments, arguments, return_type, body } => {
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
            AstNodeData::Operator { flags, operator, generic_arguments, arguments, return_type, body } => {
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
            AstNodeData::VariableDeclaration { definition, value } => NodeBuilder::new_binary("let",KEYWORD_COLOR,definition,value),
            AstNodeData::VariableDefinition { flags, name, variable_type } => {
                let mut builder = NodeBuilder::new("def",KEYWORD_COLOR);
                builder.make_field("name",name,NAME_COLOR).make_field("flags", format!("{flags}"),KEYWORD_COLOR);
                if let Some(variable_type) = variable_type {
                    builder.convert_field("type", variable_type);
                }
                builder.build()
            }
            AstNodeData::Closure { arguments, captures, return_type, body } => {
                let mut builder = NodeBuilder::new("closure", KEYWORD_COLOR);
                builder.list_field("arguments",arguments.iter()).list_field("captures",captures.iter());
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.convert_field("body", body);
                builder.build()
            }
            AstNodeData::AnonymousFunction { flags, arguments, return_type, body } => {
                let mut builder = NodeBuilder::new("anonymous function", KEYWORD_COLOR);
                builder.make_field("flags",format!("{flags}"),KEYWORD_COLOR).list_field("arguments",arguments.iter());
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.convert_field("body", body);
                builder.build()
            }
            AstNodeData::FunctionType { flags, arguments, return_type } =>
                NodeBuilder::new("fn type",TYPE_COLOR)
                    .make_field("flags",format!("{flags}"),KEYWORD_COLOR)
                    .list_field("arguments",arguments.iter())
                    .convert_field("return type",return_type)
                    .build(),
            AstNodeData::StructureDestructure(fields) => {
                let mut builder = NodeBuilder::new("structure destructure", OTHER_COLOR);
                for (name, store) in fields {
                    builder.convert_field(name,store);
                }
                builder.build()
            },
            AstNodeData::TupleDestructure(fields) => NodeBuilder::new("tuple destructure", OTHER_COLOR).add_children(fields.iter()).build(),
            AstNodeData::ArrayDestructure(fields) => NodeBuilder::new("array destructure", OTHER_COLOR).add_children(fields.iter()).build(),
            AstNodeData::SliceDestructure(fields) => NodeBuilder::new("slice destructure", OTHER_COLOR).add_children(fields.iter()).build(),
            AstNodeData::Destructure { structure, value } => NodeBuilder::new("destructure",KEYWORD_COLOR).convert_field("structure", structure).convert_field("value", value).build(),
            AstNodeData::Interface { interfaces, children, dynamic } => {
                let mut builder = NodeBuilder::new(if *dynamic { "dynamic interface" } else { "interface" }, TYPE_COLOR);
                if interfaces.len() == 0 {
                    builder.add_children(children.iter());
                } else {
                    builder.list_field("interfaces", interfaces.iter());
                    builder.list_field("children", children.iter());
                }
                builder.build()
            }
            AstNodeData::If { condition, unwrap, body, else_statement } => {
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
            AstNodeData::ComptimeIf { condition, body, else_statement } => {
                let mut builder = NodeBuilder::new("comptime if", KEYWORD_COLOR);
                builder.convert_field("condition", condition);
                builder.convert_field("body", body);
                if let Some(else_statement) = else_statement {
                    builder.convert_field("else", else_statement);
                }
                builder.build()
            }
            AstNodeData::While { condition, body, else_statement } => {
                let mut builder = NodeBuilder::new("while", KEYWORD_COLOR);
                builder.convert_field("condition", condition);
                builder.convert_field("body", body);
                if let Some(else_statement) = else_statement {
                    builder.convert_field("else", else_statement);
                }
                builder.build()
            }
            AstNodeData::For { capture, index_capture, iterable, transformations, body, else_statement } => {

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
            AstNodeData::MatchArm { matches, store, body } => {
                let mut builder = NodeBuilder::new_unnamed();
                if let Some(store) = store {
                    builder.convert_field("capture", store);
                }
                builder.list_field("matches", matches.iter());
                builder.convert_field("body", body);
                builder.build()
            }
            AstNodeData::EnumMember { name, tuple, children, value } => if children.len() == 0 && value.is_none() {
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
            AstNodeData::ArrayType { constant, dimensions, child } =>
                NodeBuilder::new("array type", TYPE_COLOR)
                    .make_field("constant", *constant, VALUE_COLOR)
                    .list_field("dimensions", dimensions.iter())
                    .convert_field("subtype", child)
                    .build(),
            AstNodeData::TypeMemberReference { referee, member } =>
                NodeBuilder::new("::",KEYWORD_COLOR)
                    .convert_field("type", referee)
                    .make_field("member", member, NAME_COLOR)
                    .build(),
            AstNodeData::GenericInstanceReference { referee, generic_args }  =>
                NodeBuilder::new("::<>",KEYWORD_COLOR)
                    .convert_field("value",referee)
                    .list_field("arguments",generic_args.iter())
                    .build(),
        }
    }
}