use std::collections::HashMap;
use std::fmt::{Arguments, Display, format, Formatter, Pointer};
use std::fs::canonicalize;
use ariadne::{Color, Fmt};
use bitflags::bitflags;
use num_bigint::BigInt;
use cheese_diagnostics::ErrorCode;
use cheese_diagnostics::locating::FileSpan;
use cheese_utilities::strings::Escapable;
use cheese_utilities::trees::{DisplayableTree, DisplayNode, NodeBuilder};

use paste::paste;
use crate::ast::AstNodeData::*;

pub fn validate_list(list: &NodeList, against: &NodeList) -> bool {
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

pub fn validate_dict(dict: &NodeDict, against: &NodeDict) -> bool {
    if dict.len() != against.len() {
        false
    } else {
        for (k,v) in dict {
            match against.get(k) {
                Option::None => return false,
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
        Option::None => true,
        Some(against) => value == against
    }
}

pub fn validate_optional(value: &OptionalNode, against: &OptionalNode) -> bool {
    match value {
        Option::None => against.is_none(),
        Some(value) => match against {
            Option::None => false,
            Some(against) => against.validate(value)
        }
    }
}


pub type NodePtr = Box<AstNode>;
pub type NodeList = Vec<NodePtr>;
pub type NodeDict = HashMap<String, NodePtr>;
pub type OptionalNode = Option<NodePtr>;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct DeclarationFlags: u16 {
        const inline   = 0b0000_0000_0000_0001;
        const external = 0b0000_0000_0000_0010;
        const export   = 0b0000_0000_0000_0100;
        const comptime = 0b0000_0000_0000_1000;
        const public   = 0b0000_0000_0001_0000;
        const private  = 0b0000_0000_0010_0000;
        const mutable  = 0b0000_0000_0100_0000;
        const entry    = 0b0000_0000_1000_0000;
        const implicit = 0b0000_0001_0000_0000;
        const explicit = 0b0000_0010_0000_0000;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConstructorType {
    New,
    Copy,
    Move,
    From,
}
impl Display for ConstructorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ConstructorType::New => "new",
            ConstructorType::Copy => "copy",
            ConstructorType::Move => "move",
            ConstructorType::From => "from",
        })
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
    MoveAssign,
    Cast,
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
            Operator::MoveAssign => "move_assign",
            Operator::Cast => "cast",
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
        if self.contains(Self::explicit) {
            write!(f, "{}explicit", prefix)?;
            prefix = " ";
        }
        if self.contains(Self::implicit) {
            write!(f, "{}implicit", prefix)?;
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
    Break,
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
    // TypeName := [identifier]
    TypeName(String),
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
    // Just a simple import statement beginning, referencing the tree
    Import(NodePtr),
    // An argument consisting of only a name
    InferredArgument(String),
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
    // NamedExpression := :name expr
    NamedExpression {
        name: String,
        expr: NodePtr,
    },
    // NamedYield := yield :name [value]
    NamedYield {
        name: String,
        value: NodePtr,
    },
    NamedBreak(String),
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
        flags: DeclarationFlags,
        name: String,
        generic_arguments: Option<NodeList>,
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
    // TypeDeclaration := type [name] is [type]

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
        default: OptionalNode
    },

    GenericArgument {
        name: String,
        argument_type: OptionalNode,
        default: OptionalNode
    },


    // project is a special node here (equivalent to crate)

    ImportStar,
    ImportTree {
        name: String,
        children: NodeList,
    },

    Structure {
        flags: DeclarationFlags,
        name: String,
        is_tuple: bool,
        generic_arguments: Option<NodeList>,
        children: NodeList,
    },

    // This is to reference another file as a module, like how rust does it
    ModuleReference {
        flags: DeclarationFlags,
        name: String,
    },

    // This declares a module inline to a file
    ModuleDeclaration {
        flags: DeclarationFlags,
        name: String,
        children: NodeList
    },

    // This is the type of files
    Module(NodeList),


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

    Constructor {
        flags: DeclarationFlags,
        cons_type: ConstructorType,
        generic_arguments: Option<NodeList>,
        arguments: NodeList,
        body: NodePtr,
    },

    ConstructorDelete {
        flags: DeclarationFlags,
        cons_type: ConstructorType,
        generic_arguments: Option<NodeList>,
        arguments: NodeList,
    },

    Destructor {
        flags: DeclarationFlags,
        body: NodePtr
    },

    Operator {
        flags: DeclarationFlags,
        operator: Operator,
        generic_arguments: Option<NodeList>,
        arguments: NodeList,
        return_type: OptionalNode, // If there is no return type it is deduced :3
        body: NodePtr,
    },

    StaticVariableDeclaration {
        flags: DeclarationFlags,
        name: String,
        variable_type: OptionalNode,
        value: NodePtr
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
        arguments: NodeList,
        return_type: OptionalNode, // If there is no return type it is assumed to be void
        body: NodePtr,
    },

    FunctionType {
        flags: DeclarationFlags,
        arguments: NodeList,
        return_type: NodePtr,
    },


    // This is using Fn instead of fn, to define a type in a Fn(...) -> ... syntax, rather than $Fn::<fn(...) -> ...>
    FunctionTraitType {
        arguments: NodeList,
        return_type: NodePtr
    },

    // This instead uses IFn instead of fn to define a type in a IFn(...) -> ... syntax, rather than $IFn::<fn(...) -> ...>
    FunctionInterfaceType {
        arguments: NodeList,
        return_type: NodePtr
    },

    // This is using Fn instead of fn, to define a type in a FnMut(...) -> ... syntax, rather than $FnMut::<fn(...) -> ...>
    MutableFunctionTraitType {
        arguments: NodeList,
        return_type: NodePtr
    },

    // This instead uses IFn instead of fn to define a type in a IFnMut(...) -> ... syntax, rather than $IFnMut::<fn(...) -> ...>
    MutableFunctionInterfaceType {
        arguments: NodeList,
        return_type: NodePtr
    },

    StructureDestructure(NodeList),

    StructureDestructureField{
        name: String,
        definition: NodePtr
    },


    TupleDestructure(NodeList),

    ArrayDestructure(NodeList),

    SliceDestructure(NodeList),

    Destructure {
        structure: NodePtr,
        value: NodePtr,
    },

    Interface {
        flags: DeclarationFlags,
        name: String,
        generic_arguments: Option<NodeList>,
        children: NodeList,
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
    },
    Tuple(NodeList)
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
            Bool => NodeBuilder::new_terminal("bool",TYPE_COLOR),
            SignedSize => NodeBuilder::new_terminal("isize",TYPE_COLOR),
            UnsignedSize => NodeBuilder::new_terminal("usize",TYPE_COLOR),
            Float32 => NodeBuilder::new_terminal("f32",TYPE_COLOR),
            Float64 => NodeBuilder::new_terminal("f64",TYPE_COLOR),
            Complex32 => NodeBuilder::new_terminal("c32",TYPE_COLOR),
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
            AstNodeData::Break => NodeBuilder::new_terminal("break", KEYWORD_COLOR),
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
            AstNodeData::TupleLiteral(children) => NodeBuilder::new("tuple literal",OTHER_COLOR).add_children(children).build(),
            AstNodeData::ArrayLiteral(children) => NodeBuilder::new("array literal",OTHER_COLOR).add_children(children).build(),
            AstNodeData::EnumLiteral(name) => NodeBuilder::new("enum literal",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::BuiltinReference(name) => NodeBuilder::new("builtin reference",OTHER_COLOR).make_inline(name, METHOD_COLOR).build(),
            AstNodeData::CopyCapture(name) => NodeBuilder::new("copy capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::ReferenceCapture(name) => NodeBuilder::new("reference capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::ConstantReferenceCapture(name) => NodeBuilder::new("constant reference capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::ObjectLiteral(children) => NodeBuilder::new("object block",OTHER_COLOR).add_children(children).build(),
            AstNodeData::Block(children) => NodeBuilder::new("block",KEYWORD_COLOR).add_children(children).build(),
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
            AstNodeData::NamedBreak(name) => NodeBuilder::new_terminal(name,METHOD_COLOR),
            AstNodeData::TupleCall { functional, args } =>
                NodeBuilder::new("()",KEYWORD_COLOR)
                    .convert_field("value",functional)
                    .list_field("arguments",args)
                    .build(),
            AstNodeData::ArrayCall { functional, args } =>
                NodeBuilder::new("[]",KEYWORD_COLOR)
                    .convert_field("value",functional)
                    .list_field("arguments",args)
                    .build(),
            AstNodeData::NamedExpression { name, expr } =>
                NodeBuilder::new("named expression",KEYWORD_COLOR)
                    .make_field("name",name,METHOD_COLOR)
                    .convert_field("expression",expr)
                    .build(),
            AstNodeData::NamedYield { name, value } =>
                NodeBuilder::new("named yield",KEYWORD_COLOR)
                    .make_field("name",name,METHOD_COLOR)
                    .convert_field("value",value)
                    .build(),
            AstNodeData::ObjectCall { functional, args } =>
                NodeBuilder::new("{}",KEYWORD_COLOR)
                    .convert_field("value",functional)
                    .list_field("arguments",args)
                    .build(),
            AstNodeData::ErrorNode { message, .. } => NodeBuilder::new_terminal(message,Color::BrightRed),
            AstNodeData::Match { value, arms } =>
                NodeBuilder::new("match",KEYWORD_COLOR)
                    .convert_field("value",value)
                    .list_field("arms",arms)
                    .build(),
            AstNodeData::MatchRange { begin, end } => NodeBuilder::new_binary("match range",KEYWORD_COLOR,begin,end),
            AstNodeData::DestructuringMatchArm { matches, store } => {
                let mut builder = NodeBuilder::new("destructuring match",KEYWORD_COLOR);
                match store {
                    Option::None => builder.make_field("store into", "_",NAME_COLOR),
                    Some(store) => builder.convert_field("store into",store),
                }.list_field("matches",matches).build()
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
                    .list_field("fields",children)
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
                    .add_children(matches)
                    .build(),
            AstNodeData::DestructuringMatchArray(matches) =>
                NodeBuilder::new("array match",KEYWORD_COLOR)
                    .add_children(matches)
                    .build(),
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
                    builder.list_field("generic arguments",args);
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
            AstNodeData::Argument { name, argument_type, default } => match name {
                Some(n) => {
                    match default {
                        Some(d) => {
                            NodeBuilder::new_unnamed()
                                .make_field("name", n, NAME_COLOR)
                                .convert_field("type", argument_type)
                                .convert_field("default", d)
                                .build()
                        }
                        _ => {
                            NodeBuilder::new_unnamed()
                                .make_field("name", n, NAME_COLOR)
                                .convert_field("type", argument_type)
                                .build()
                        }
                    }
                }
                _ => {
                    match default {
                        Some(d) => {
                            NodeBuilder::new_unnamed()
                                .convert_field("type", argument_type)
                                .convert_field("default", d)
                                .build()
                        },
                        _ => {
                            argument_type.to_node()
                        }
                    }
                }
            },
            AstNodeData::Structure { is_tuple, name, generic_arguments, flags , children } => {
                let mut builder = NodeBuilder::new(if *is_tuple { "tuple" } else { "structure" }, TYPE_COLOR);
                builder.make_field("name", name, TYPE_COLOR);
                builder.make_field("flags", format!("{flags}"),KEYWORD_COLOR);
                if let Some(generic_arguments) = generic_arguments {
                    builder.list_field("generic arguments", generic_arguments);
                }
                builder.list_field("children", children);
                builder.build()
            }
            AstNodeData::FunctionPrototype { flags, name, arguments, return_type } => {
                let mut builder = NodeBuilder::new("fn prototype", KEYWORD_COLOR);
                builder.make_field("name",name, METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR).list_field("arguments",arguments);
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.build()
            }
            AstNodeData::FunctionImport{ flags, name, arguments, return_type , import_name} => {
                let mut builder = NodeBuilder::new("fn import", KEYWORD_COLOR);
                builder.make_field("name",name, METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR).list_field("arguments",arguments);
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
                    builder.list_field("generic arguments",args);
                }
                builder.list_field("arguments",arguments);
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
                    builder.list_field("generic arguments",args);
                }
                builder.list_field("arguments",arguments);
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
                builder.list_field("arguments",arguments).list_field("captures",captures);
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.convert_field("body", body);
                builder.build()
            }
            AstNodeData::AnonymousFunction {  arguments, return_type, body } => {
                let mut builder = NodeBuilder::new("anonymous function", KEYWORD_COLOR);
                builder.list_field("arguments",arguments);
                if let Some(ret) = return_type {
                    builder.convert_field("return type", ret);
                }
                builder.convert_field("body", body);
                builder.build()
            }
            AstNodeData::FunctionType { flags, arguments, return_type } =>
                NodeBuilder::new("fn type",TYPE_COLOR)
                    .make_field("flags",format!("{flags}"),KEYWORD_COLOR)
                    .list_field("arguments",arguments)
                    .convert_field("return type",return_type)
                    .build(),
            AstNodeData::StructureDestructure(fields) => {
                let mut builder = NodeBuilder::new("structure destructure", OTHER_COLOR);
                for field in fields {
                    builder.convert_child(field);
                }
                builder.build()
            },
            AstNodeData::TupleDestructure(fields) => NodeBuilder::new("tuple destructure", OTHER_COLOR).add_children(fields).build(),
            AstNodeData::ArrayDestructure(fields) => NodeBuilder::new("array destructure", OTHER_COLOR).add_children(fields).build(),
            AstNodeData::SliceDestructure(fields) => NodeBuilder::new("slice destructure", OTHER_COLOR).add_children(fields).build(),
            AstNodeData::Destructure { structure, value } => NodeBuilder::new("destructure",KEYWORD_COLOR).convert_field("structure", structure).convert_field("value", value).build(),
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
                    builder.list_field("transformations", transformations);
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
                builder.list_field("matches", matches);
                builder.convert_field("body", body);
                builder.build()
            }
            AstNodeData::EnumMember { name, tuple, children, value } => if children.len() == 0 && value.is_none() {
                NodeBuilder::new_terminal(name, NAME_COLOR)
            } else {
                let mut builder = NodeBuilder::new_unnamed();
                builder.make_field("name",name,NAME_COLOR);
                if children.len() > 0 {
                    builder.list_field(if *tuple { "tuple" } else {"structure"},children);
                }
                if let Some(value) = value {
                    builder.convert_field("value", value);
                }
                builder.build()
            },
            AstNodeData::ArrayType { constant, dimensions, child } =>
                NodeBuilder::new("array type", TYPE_COLOR)
                    .make_field("constant", *constant, VALUE_COLOR)
                    .list_field("dimensions", dimensions)
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
                    .list_field("arguments",generic_args)
                    .build(),
            AstNodeData::Constructor { flags, cons_type, generic_arguments, arguments, body } => {
                let mut builder = NodeBuilder::new("constructor", KEYWORD_COLOR);
                builder.make_field("type",format!("{cons_type}"), METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR);
                if let Some(args) = generic_arguments {
                    builder.list_field("generic arguments",args);
                }
                builder.list_field("arguments",arguments);
                builder.convert_field("body", body);
                builder.build()
            }
            AstNodeData::ConstructorDelete { flags, cons_type, generic_arguments, arguments } => {
                let mut builder = NodeBuilder::new("constructor delete", KEYWORD_COLOR);
                builder.make_field("type",format!("{cons_type}"), METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR);
                if let Some(args) = generic_arguments {
                    builder.list_field("generic arguments",args);
                }
                builder.list_field("arguments",arguments);
                builder.build()
            }
            AstNodeData::Destructor { flags, body }  => {
                let mut builder = NodeBuilder::new("destructor", KEYWORD_COLOR);
                builder.make_field("flags",format!("{flags}"),KEYWORD_COLOR);
                builder.convert_field("body", body);
                builder.build()
            }
            Enum { flags, name, containing_type, children, generic_arguments } => {
                let mut builder = NodeBuilder::new("enum",KEYWORD_COLOR);
                builder.make_field("name",name,TYPE_COLOR);
                builder.make_field("flags", format!("{flags}"), KEYWORD_COLOR);
                if let Some(containing_type) = containing_type {
                    builder.convert_field("containing type",containing_type);
                }
                if let Some(generic_arguments) = generic_arguments {
                    builder.list_field("generic arguments", generic_arguments);
                }
                builder.list_field("children",children).build()
            }
            ImportStar => {
                NodeBuilder::new("*",TYPE_COLOR).build()
            }
            ImportTree { name, children } => {
                NodeBuilder::new(name,TYPE_COLOR).add_children(children).build()
            }
            ModuleReference { flags, name } => NodeBuilder::new("module reference",KEYWORD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR).make_field("name",name,TYPE_COLOR).build(),
            ModuleDeclaration { flags, name, children } => NodeBuilder::new("module declaration",KEYWORD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR).make_field("name",name,TYPE_COLOR).list_field("children",children).build(),
            Module(children) => NodeBuilder::new("module",KEYWORD_COLOR).add_children(children).build(),
            StaticVariableDeclaration { flags, name, variable_type, value } => {
                let mut builder = NodeBuilder::new("static variable",KEYWORD_COLOR);
                builder.make_field("flags",format!("{flags}"),KEYWORD_COLOR).make_field("name",name,NAME_COLOR);
                if let Some(variable_type) = variable_type {
                    builder.convert_field("type", variable_type);
                }
                builder.convert_field("value",value).build()
            }
            FunctionTraitType { arguments, return_type } => NodeBuilder::new("Fn",TYPE_COLOR).list_field("arguments",arguments).convert_field("return type",return_type).build(),
            FunctionInterfaceType { arguments, return_type } => NodeBuilder::new("IFn",TYPE_COLOR).list_field("arguments",arguments).convert_field("return type",return_type).build(),
            MutableFunctionTraitType { arguments, return_type } => NodeBuilder::new("FnMut",TYPE_COLOR).list_field("arguments",arguments).convert_field("return type",return_type).build(),
            MutableFunctionInterfaceType { arguments, return_type } => NodeBuilder::new("IFnMut",TYPE_COLOR).list_field("arguments",arguments).convert_field("return type",return_type).build(),
            Interface { flags, name, generic_arguments, children } => {
                let mut builder = NodeBuilder::new("interface", KEYWORD_COLOR);
                builder.make_field("flags", format!("{flags}"), KEYWORD_COLOR).make_field("name", name, TYPE_COLOR);
                if let Some(generic_arguments) = generic_arguments {
                    builder.list_field("generic arguments", generic_arguments);
                }
                builder.list_field("children", children).build()
            }
            Import(tree) => NodeBuilder::new("import",KEYWORD_COLOR).convert_inline(tree).build(),
            TypeName(name) => NodeBuilder::new(name,TYPE_COLOR).build(),
            Tuple(children) => NodeBuilder::new("tuple",TYPE_COLOR).add_children(children).build(),
            InferredArgument(name) => NodeBuilder::new(name, NAME_COLOR).build(),
            StructureDestructureField { name, definition } => NodeBuilder::new(name, NAME_COLOR).convert_child(definition).build(),
            GenericArgument { name, argument_type, default } => {
                let mut builder = NodeBuilder::new_unnamed();
                builder.make_field("name", name, TYPE_COLOR);
                match argument_type {
                    Some(t) => {
                        builder.convert_field("type", t);
                    },
                    _ => {}
                }
                match default {
                    Some(d) => {
                        builder.convert_field("default", d);
                    },
                    _ => {}
                }
                builder.build()
            }
        }
    }
}

impl AstNodeData {
    pub fn validate(&self, node: &NodePtr) -> bool {
        let data = &node.data;
        macro_rules! check {
            ($name:ident) => {
                if let $name = data { true } else { false }
            };
            ($name:ident, $value:expr) => {
                if let $name(v) = data { v == $value } else { false }
            };
            ($name:ident, $lhs:expr, $rhs:expr) => {
                check!($name,lhs,$lhs,rhs,$rhs)
            };
            ($name:ident, $lhs_name:ident, $lhs:expr, $rhs_name:ident, $rhs:expr) => {
                if let $name{
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
                if let $name(v) = &data { validate_list(v,$validators)} else { false }
            }
        }
        macro_rules! check_dict {
            ($name: ident, $validators:expr) => {
                if let $name(v) = &data { validate_dict(v,$validators)} else { false }
            };
        }
        macro_rules! check_unary {
            ($name: ident, $validator:expr) => {
                if let $name(v) = &data { $validator.validate(v) } else { false }
            };
        }
        macro_rules! check_functional {
            ($name: ident ($args:expr) -> $rt:expr) => {
                if let $name{
                    arguments: a,
                    return_type: b
                } = &data {
                    validate_list(a,$args) && $rt.validate(b)
                } else {
                    false
                }
            }
        }
        let result = match self {
            Bool => check!(Bool),
            SignedSize => check!(SignedSize),
            UnsignedSize => check!(UnsignedSize),
            Float32 => check!(Float32),
            Float64 => check!(Float64),
            Complex32 => check!(Complex32),
            Complex64 => check!(Complex64),
            Opaque => check!(Opaque),
            Void => check!(Void),
            CompileTimeFloat => check!(CompileTimeFloat),
            CompileTimeComplex => check!(CompileTimeComplex),
            CompileTimeString => check!(CompileTimeString),
            CompileTimeInteger => check!(CompileTimeInteger),
            Type => check!(Type),
            NoReturn => check!(NoReturn),
            True => check!(True),
            False => check!(False),
            None => check!(None),
            Underscore => check!(Underscore),
            Continue => check!(Continue),
            Break => check!(Break),
            EmptyReturn => check!(EmptyReturn),
            SelfValue => check!(SelfValue),
            ConstSelfValue => check!(ConstSelfValue),
            SelfType => check!(SelfType),
            ConstReferenceImplicitCapture => check!(ConstReferenceImplicitCapture),
            ReferenceImplicitCapture => check!(ReferenceImplicitCapture),
            CopyImplicitCapture => check!(CopyImplicitCapture),
            MatchAll => check!(MatchAll),
            NonExhaustive => check!(NonExhaustive),
            UnknownSize => check!(UnknownSize),
            InferredSize => check!(InferredSize),
            SignedIntegerType(sz) => check!(SignedIntegerType,sz),
            UnsignedIntegerType(sz) => check!(UnsignedIntegerType,sz),
            StringLiteral(s) => check!(StringLiteral,s),
            FloatLiteral(f) => check!(FloatLiteral,f),
            ImaginaryLiteral(i) => check!(ImaginaryLiteral, i),
            IntegerLiteral(i) => if let IntegerLiteral(other) = data {
                other.eq(i)
            } else {
                false
            },
            NameReference(name) => check!(NameReference, name),
            TupleLiteral(args) => check_list!(TupleLiteral, args),
            ArrayLiteral(args) => check_list!(ArrayLiteral, args),
            EnumLiteral(name) => check!(EnumLiteral, name),
            BuiltinReference(name) => check!(BuiltinReference, name),
            CopyCapture(name) => check!(CopyCapture, name),
            ReferenceCapture(name) => check!(ReferenceCapture, name),
            ConstantReferenceCapture(name) => check!(ConstantReferenceCapture, name),
            ObjectLiteral(args) => check_list!(ObjectLiteral,args),
            Block(list) => check_list!(Block, list),
            Return(arg) => check_unary!(Return, arg),
            Yield(arg) => check_unary!(Yield, arg),
            Not(arg) => check_unary!(Not, arg),
            UnaryMinus(arg) => check_unary!(UnaryMinus, arg),
            UnaryPlus(arg) => check_unary!(UnaryPlus, arg),
            Dereference(arg) => check_unary!(Dereference, arg),
            AddressOf(arg) => check_unary!(AddressOf, arg),
            Concept(arg) => check_unary!(Concept, arg),
            Loop(arg) => check_unary!(Loop, arg),
            FilterTransformation(arg) => check_unary!(FilterTransformation, arg),
            MapTransformation(arg) => check_unary!(MapTransformation, arg),
            Comptime(arg) => check_unary!(Comptime, arg),
            ImplicitResult(arg) => check_unary!(ImplicitResult,arg),
            Typeof(arg) => check_unary!(Typeof,arg),
            NamedBreak(name) => check!(NamedBreak, name),
            ImplicitArray {
                constant, subtype
            } => {
                if let ImplicitArray { constant: c2, subtype: s2 } = &data {
                    *constant == *c2 && subtype.validate(s2)
                } else {
                    false
                }
            },
            Slice {
                constant, subtype
            } => {
                if let Slice { constant: c2, subtype: s2 } = &data {
                    *constant == *c2 && subtype.validate(s2)
                } else {
                    false
                }
            },
            Reference {
                constant, subtype
            } => {
                if let Reference { constant: c2, subtype: s2 } = &data {
                    *constant == *c2 && subtype.validate(s2)
                } else {
                    false
                }
            },
            TupleCall { functional, args } => {
                if let TupleCall {
                    functional: f2,
                    args: a2
                } = &data {
                    functional.validate(f2) && validate_list(a2, args)
                } else {
                    false
                }
            },
            ArrayCall { functional, args } => {
                if let ArrayCall {
                    functional: f2,
                    args: a2
                } = &data {
                    functional.validate(f2) && validate_list(a2, args)
                } else {
                    false
                }
            },
            NamedExpression { name, expr } => {
                if let NamedExpression {
                    name: n2,
                    expr: e2,
                } = &data {
                    name == n2 && expr.validate(e2)
                } else {
                    false
                }
            },
            NamedYield { name, value } => {
                if let NamedYield {
                    name: n2,
                    value: v2
                } = &data {
                    name == n2 && value.validate(v2)
                } else {
                    false
                }
            },
            ObjectCall { functional, args } => {
                if let ObjectCall {
                    functional: f2,
                    args: a2
                } = &data {
                    functional.validate(f2) && validate_list(a2, args)
                } else {
                    false
                }
            },
            Match { value, arms } => {
                if let Match {
                    value: v2,
                    arms: a2
                } = &data {
                    value.validate(v2) && validate_list(a2, arms)
                } else {
                    false
                }
            },
            MatchRange { begin, end } => check!(MatchRange,begin,begin,end,end),
            DestructuringMatchArm { matches, store } => {
                if let DestructuringMatchArm {
                    matches: m2,
                    store: s2
                } = &data {
                    validate_optional(s2,store) && validate_list(m2, matches)
                } else {
                    false
                }
            },
            MatchEnumStructure { enum_identifier, children } => {
                if let MatchEnumStructure {
                    enum_identifier: e2,
                    children: c2
                } = &data {
                    e2 == enum_identifier && validate_dict(c2,children)
                } else {
                    false
                }
            },
            MatchEnumTuple { enum_identifier, children }  => {
                if let MatchEnumTuple {
                    enum_identifier: e2,
                    children: c2
                } = &data {
                    e2 == enum_identifier && validate_list(c2,children)
                } else {
                    false
                }
            },
            MatchValue(value) => check_unary!(MatchValue,value),
            MatchConstraint(value) => check_unary!(MatchConstraint,value),
            DestructuringMatchStructure(value) => check_dict!(DestructuringMatchStructure,value),
            DestructuringMatchTuple(value) => check_list!(DestructuringMatchTuple,value),
            DestructuringMatchArray(value) => check_list!(DestructuringMatchArray,value),
            FieldLiteral { name, value } => {
                if let FieldLiteral {
                    name: n2,
                    value: v2
                } = &data {
                    name == n2 && value.validate(v2)
                } else {
                    false
                }
            }
            Subscription { lhs, rhs } => check!(Subscription, lhs, rhs),
            Multiplication { lhs, rhs } => check!(Multiplication, lhs, rhs),
            Division { lhs, rhs } => check!(Division, lhs, rhs),
            Modulus { lhs, rhs } => check!(Modulus, lhs, rhs),
            Addition { lhs, rhs } => check!(Addition, lhs, rhs),
            Subtraction { lhs, rhs } => check!(Subtraction, lhs, rhs),
            LeftShift { lhs, rhs } => check!(LeftShift, lhs, rhs),
            RightShift { lhs, rhs } => check!(RightShift, lhs, rhs),
            LesserThan { lhs, rhs } => check!(LesserThan, lhs, rhs),
            GreaterThan { lhs, rhs } => check!(GreaterThan, lhs, rhs),
            LesserEqual { lhs, rhs } => check!(LesserEqual, lhs, rhs),
            GreaterEqual { lhs, rhs } => check!(GreaterEqual, lhs, rhs),
            EqualTo { lhs, rhs } => check!(EqualTo, lhs, rhs),
            NotEqualTo { lhs, rhs } => check!(NotEqualTo, lhs, rhs),
            And { lhs, rhs } => check!(And, lhs, rhs),
            Or { lhs, rhs } => check!(Or, lhs, rhs),
            Xor { lhs, rhs } => check!(Xor, lhs, rhs),
            Combine { lhs, rhs } => check!(Combine, lhs, rhs),
            Reassign { lhs, rhs } => check!(Reassign, lhs, rhs),
            Assign { lhs, rhs } => check!(Assign, lhs, rhs),
            AddAssign { lhs, rhs } => check!(AddAssign, lhs, rhs),
            SubtractAssign { lhs, rhs } => check!(SubtractAssign, lhs, rhs),
            MultiplyAssign { lhs, rhs } => check!(MultiplyAssign, lhs, rhs),
            DivideAssign { lhs, rhs } => check!(DivideAssign, lhs, rhs),
            ModulateAssign { lhs, rhs } => check!(ModulateAssign, lhs, rhs),
            ShiftLeftAssign { lhs, rhs } => check!(ShiftLeftAssign, lhs, rhs),
            ShiftRightAssign { lhs, rhs } => check!(ShiftRightAssign, lhs, rhs),
            AndAssign { lhs, rhs } => check!(AndAssign, lhs, rhs),
            OrAssign { lhs, rhs } => check!(OrAssign, lhs, rhs),
            XorAssign { lhs, rhs } => check!(XorAssign, lhs, rhs),
            IsType { lhs, rhs } => check!(SubtractAssign, lhs, rhs),
            DynamicCast { lhs, to } => check!(DynamicCast, lhs, lhs, to, to),
            Cast { lhs, to } => check!(Cast, lhs, lhs, to, to),
            Range { begin, end } => check!(Range, begin, begin, end, end),
            TypeDeclaration { flags, name, generic_arguments, alias } => {
                if let TypeDeclaration {
                    flags: f2,
                    name: n2,
                    generic_arguments: g2,
                    alias: a2
                } = &data {
                    f2 == flags && name == n2 && match generic_arguments {
                        Option::None => g2.is_none(),
                        Some(v) => match g2 {
                            Option::None => false,
                            Some(v2) => validate_list(v2, v)
                        }
                    } && alias.validate(a2)
                } else {
                    false
                }
            }
            Field { flags, name, field_type } => {
                if let Field {
                    flags: f2,
                    name: n2,
                    field_type: ft2
                } = &data {
                    f2 == flags && name == n2 && field_type.validate(ft2)
                } else {
                    false
                }
            }
            Argument { name, argument_type, default } => {
                if let Argument {
                    name: n2, argument_type: a2, default: d2
                } = &data {
                    name == n2 && argument_type.validate(a2) && validate_optional(default, d2)
                } else {
                    false
                }
            }
            FunctionPrototype { flags, name, arguments, return_type } => {
                if let FunctionPrototype {
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
            FunctionImport { flags, name, arguments, return_type, import_name } => {
                if let FunctionImport {
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
            Function { flags, name, generic_arguments, arguments, return_type, body } => {
                if let Function {
                    flags: f2,
                    name: n2,
                    generic_arguments: g2,
                    arguments: a2,
                    return_type: r2,
                    body: b2
                } = &data {
                    f2 == flags && name == n2 && match generic_arguments {
                        Option::None => g2.is_none(),
                        Some(v) => match g2 {
                            Option::None => false,
                            Some(v2) => validate_list(v2, v)
                        }
                    } && validate_list(a2,arguments) && validate_optional(r2, return_type) && body.validate(b2)
                } else {
                    false
                }
            },
            Constructor {
                flags, cons_type, generic_arguments, arguments, body
            } => {
                if let Constructor {
                    flags: f2,
                    cons_type: c2,
                    generic_arguments: g2,
                    arguments: a2,
                    body: b2
                } = &data {
                    f2 == flags && cons_type == c2 && match generic_arguments {
                        Option::None => g2.is_none(),
                        Some(v) => match g2 {
                            Option::None => false,
                            Some(v2) => validate_list(v2, v)
                        }
                    } && validate_list(a2,arguments) && body.validate(b2)
                } else {
                    false
                }
            },
            ConstructorDelete {
                flags, cons_type, generic_arguments, arguments
            } => {
                if let ConstructorDelete {
                    flags: f2,
                    cons_type: c2,
                    generic_arguments: g2,
                    arguments: a2,
                } = &data {
                    f2 == flags && cons_type == c2 && match generic_arguments {
                        Option::None => g2.is_none(),
                        Some(v) => match g2 {
                            Option::None => false,
                            Some(v2) => validate_list(v2, v)
                        }
                    } && validate_list(a2,arguments)
                } else {
                    false
                }
            },
            Destructor {
                flags, body
            } => {
                if let Destructor {
                    flags: f2,
                    body: b2
                } = &data {
                    f2 == flags && body.validate(b2)
                } else {
                    false
                }
            },
            AstNodeData::Operator { flags, operator, generic_arguments, arguments, return_type, body } => {
                if let AstNodeData::Operator {
                    flags: f2,
                    operator: o2,
                    generic_arguments: g2,
                    arguments: a2,
                    return_type: r2,
                    body: b2
                } = &data {
                    f2 == flags && operator == o2 && match generic_arguments {
                        Option::None => g2.is_none(),
                        Some(v) => match g2 {
                            Option::None => false,
                            Some(v2) => validate_list(v2, v)
                        }
                    } && validate_list(a2,arguments) && validate_optional(r2, return_type) && body.validate(b2)
                } else {
                    false
                }
            }
            VariableDeclaration { definition, value } => check!(VariableDeclaration,definition,definition,value,value),
            VariableDefinition { flags, name, variable_type } => {
                if let VariableDefinition {
                    flags: f2,
                    name: n2,
                    variable_type: v2
                } = &data {
                    flags == f2 && name == n2 && validate_optional(v2, variable_type)
                } else {
                    false
                }
            }
            Closure { arguments, captures, return_type, body } => {
                if let Closure {
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
            AnonymousFunction { arguments, return_type, body } => {
                if let AnonymousFunction {
                    arguments: a2,
                    return_type: r2,
                    body: b2
                } = &data {
                    validate_list(a2,arguments) && validate_optional(r2, return_type) && body.validate(b2)
                } else {
                    false
                }
            }
            FunctionType { flags, arguments, return_type } => {
                if let FunctionType {
                    flags: f2,
                    arguments: a2,
                    return_type: r2,
                } = &data {
                    f2 == flags && validate_list(a2,arguments) && return_type.validate(r2)
                } else {
                    false
                }
            }
            StructureDestructure(list) => check_list!(StructureDestructure, list),
            TupleDestructure(list) => check_list!(TupleDestructure, list),
            ArrayDestructure(list) => check_list!(ArrayDestructure, list),
            SliceDestructure(list) => check_list!(SliceDestructure, list),
            Destructure { structure, value } => check!(Destructure,structure,structure,value,value),
            If { condition, unwrap, body, else_statement } => {
                if let If {
                    condition: c2, unwrap: u2, body: b2, else_statement: e2
                } = &data {
                    condition.validate(c2) && validate_optional(u2, unwrap) && body.validate(b2) && validate_optional(e2, else_statement)
                } else {
                    false
                }
            }
            ComptimeIf { condition, body, else_statement } => {
                if let ComptimeIf {
                    condition: c2, body: b2, else_statement: e2
                } = &data {
                    condition.validate(c2) && body.validate(b2) && validate_optional(e2, else_statement)
                } else {
                    false
                }
            }
            While { condition, body, else_statement } => {
                if let While {
                    condition: c2, body: b2, else_statement: e2
                } = &data {
                    condition.validate(c2) && body.validate(b2) && validate_optional(e2, else_statement)
                } else {
                    false
                }
            }
            For { capture, index_capture, iterable, transformations, body, else_statement } => {
                if let For {
                    capture:c2, index_capture:ic2, iterable:i2, transformations:t2, body:b2, else_statement:e2
                } = &data {
                    capture.validate(c2) && validate_optional(ic2, index_capture) && iterable.validate(i2) && validate_list(t2, transformations) && body.validate(b2) && validate_optional(e2, else_statement)
                } else {
                    false
                }
            }
            MatchArm { matches, store, body } => {
                if let MatchArm {
                    matches: m2, store: s2, body: b2
                } = &data {
                    validate_list(m2, matches) && validate_optional(s2, store) && body.validate(b2)
                } else {
                    false
                }
            }
            EnumMember { name, tuple, children, value } => {
                if let EnumMember {
                    name: n2, tuple: t2, children: c2, value: v2
                } = &data {
                    tuple == t2 && name == n2 && validate_list(c2, children) && validate_optional(v2, value)
                } else {
                    false
                }
            }
            ArrayType { constant, dimensions, child } => {
                if let ArrayType {
                    constant:c2, dimensions: d2, child: c3
                } = &data {
                    constant == c2 && validate_list(d2, dimensions) && child.validate(c3)
                } else {
                    false
                }
            }
            TypeMemberReference { referee, member } => {
                if let TypeMemberReference {
                    referee: r2, member: m2
                } = &data {
                    m2 == member && referee.validate(r2)
                } else {
                    false
                }
            }
            GenericInstanceReference { referee, generic_args } => {
                if let GenericInstanceReference { referee: r2, generic_args: g2 } = &data {
                    referee.validate(r2) && validate_list(g2, generic_args)
                } else {
                    false
                }
            }
            ErrorNode { .. } => true,
            Import(tree) => check_unary!(Import,tree),
            Enum { flags, name, generic_arguments, containing_type, children } => {
                if let Enum{flags: f2, name: n2, generic_arguments: g2, containing_type: ct2, children: c2} = &data {
                    flags == f2 && name == n2 && match generic_arguments {
                        Option::None => g2.is_none(),
                        Some(args) => match g2 {
                            Option::None => false,
                            Some(args2) => validate_list(args,args2)
                        }
                    } && match containing_type {
                        OptionalNode::None => ct2.is_none(),
                        Some(ct) => match ct2 {
                            OptionalNode::None => false,
                            Some(ct2) => ct.validate(ct2)
                        }
                    } && validate_list(children,c2)
                } else {
                    false
                }
            }
            ImportStar => check!(ImportStar),
            ImportTree { name, children } => {
                if let ImportTree { name: n2, children: c2} = &data {
                    name == n2 && validate_list(children, c2)
                } else {
                    false
                }
            }
            Structure { flags, name, is_tuple, generic_arguments, children } => {
                if let Structure{flags: f2, name: n2,  is_tuple: t2, generic_arguments: g2, children: c2} = &data {
                    flags == f2 && name == n2 && is_tuple == t2 && match generic_arguments {
                        Option::None => g2.is_none(),
                        Some(args) => match g2 {
                            Option::None => false,
                            Some(args2) => validate_list(args,args2)
                        }
                    } && validate_list(children, c2)
                } else {
                    false
                }
            }
            ModuleReference { flags, name } => {
                if let ModuleReference { flags: f2, name: n2} = &data {
                    flags == f2 && name == n2
                } else {
                    false
                }
            }
            ModuleDeclaration { flags, name, children } => {
                if let ModuleDeclaration { flags: f2, name: n2, children: c2} = &data {
                    flags == f2 && name == n2 && validate_list(children,c2)
                } else {
                    false
                }
            }
            Module(children) => check_list!(Module,children),
            StaticVariableDeclaration { flags, name, variable_type, value } => {
                if let StaticVariableDeclaration {flags: f2, name: n2, variable_type: t2, value: v2} = &data {
                    flags == f2 && name == n2 && match variable_type {
                        OptionalNode::None => t2.is_none(),
                        Some(t1) => match t2 {
                            OptionalNode::None => false,
                            Some(t2) => t1.validate(t2)
                        }
                    }
                } else {
                    false
                }
            }
            FunctionTraitType { arguments, return_type } => check_functional!(FunctionTraitType(arguments) -> return_type),
            FunctionInterfaceType { arguments, return_type } => check_functional!(FunctionInterfaceType(arguments) -> return_type),
            MutableFunctionTraitType { arguments, return_type } => check_functional!(MutableFunctionTraitType(arguments) -> return_type),
            MutableFunctionInterfaceType { arguments, return_type } => check_functional!(MutableFunctionInterfaceType(arguments) -> return_type),
            Interface { flags, name, generic_arguments, children } => {
                if let Interface {flags: f2, name: n2, generic_arguments: g2, children: c2} = &data {
                    flags == f2 && name == n2 && match generic_arguments {
                        Option::None => g2.is_none(),
                        Some(args) => match g2 {
                            Option::None => false,
                            Some(args2) => validate_list(args,args2)
                        }
                    } && validate_list(children, c2)
                } else {
                    false
                }
            }
            TypeName(name) => check!(TypeName, name),
            Tuple(children) => check_list!(Tuple, children),
            InferredArgument(name) => check!(InferredArgument, name),
            StructureDestructureField { name, definition } => {
                if let StructureDestructureField {name: n2, definition: d2} = &data {
                    name == n2 && definition.validate(d2)
                } else {
                    false
                }
            }
            GenericArgument { name, argument_type, default } => {
                if let GenericArgument {name: n2, argument_type: t2, default: d2} = &data {
                    name == n2 && validate_optional(argument_type, t2) && validate_optional(default,d2)
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

impl AstNode {
    pub fn validate(&self, node: &NodePtr) -> bool {
        self.data.validate(node)
    }
}

macro_rules! validator_field {
    ($field:ident,String) => {
        $field.to_string()
    };
    ($field:ident,Option<String>) => {
        match $field {
            Some(s) => Some(s.to_string()),
            Option::None => Option::None
        }
    };
    ($field:ident,$t:ty) => {
        $field
    };
    (String) => {
        impl ToString
    };
    (Option<String>) => {
        Option<impl ToString>
    };
    ($t:ty) => {
        $t
    };
}
macro_rules! validator {
    ($name:ident) => {
        paste! {
            pub fn [<v_ $name:snake >]  () -> NodePtr {
                AstNode::new(FileSpan::default(),AstNodeData::$name)
            }
        }
    };
    ($name:ident(String)) => {
        paste! {
            pub fn [<v_ $name:snake >](v: impl ToString) -> NodePtr {
                AstNode::new(FileSpan::default(),AstNodeData::$name(v.to_string()))
            }
        }
    };
    ($name:ident($t:ty)) => {
        paste! {
            pub fn [<v_ $name:snake >]  (v: $t) -> NodePtr {
                AstNode::new(FileSpan::default(),AstNodeData::$name(v))
            }
        }
    };

    ($name:ident, $args:tt, $body:expr) => {
        pub fn $name $args -> NodePtr {
            $body
        }
    };

    ($name:ident{$($field:ident:$t:ty),*$(,)?}) => {
        // paste! {pub fn [<v_ $name:snake >]}
        paste! {
            validator!{
                [<v_ $name:snake >],
                (
                    $(
                       $field: validator_field!($t)
                    ),*
                ),
                AstNode::new(FileSpan::default(),AstNodeData::$name{
                    $(
                        $field: validator_field!($field,$t)
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
    Break,
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
    ImportStar
);
validator_set!(
    SignedIntegerType(u64),
    UnsignedIntegerType(u64),
    StringLiteral(String),
    FloatLiteral(f64),
    ImaginaryLiteral(f64),
    IntegerLiteral(BigInt),
    NameReference(String),
    TupleLiteral(NodeList),
    ArrayLiteral(NodeList),
    EnumLiteral(String),
    BuiltinReference(String),
    CopyCapture(String),
    ReferenceCapture(String),
    ConstantReferenceCapture(String),
    ObjectLiteral(NodeList),
    Block(NodeList),
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
    StructureDestructure(NodeList),
    TupleDestructure(NodeList),
    ArrayDestructure(NodeList),
    SliceDestructure(NodeList),
    Typeof(NodePtr),
    Module(NodeList),
    TypeName(String),
    Tuple(NodeList),
    NamedBreak(String),
    InferredArgument(String),
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
validator!(NamedExpression {
        name: String,
        expr: NodePtr
    });
validator!(NamedYield {
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
validator!(Argument{
        name: Option<String>,
        argument_type: NodePtr,
        default: OptionalNode
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

validator!(Enum {
        flags: DeclarationFlags,
        name: String,
        generic_arguments: Option<NodeList>,
        containing_type: OptionalNode,
        children: NodeList,
});

validator!(StaticVariableDeclaration {
    flags: DeclarationFlags,
    name: String,
    variable_type: OptionalNode,
    value: NodePtr
});

validator!(Structure {
        flags: DeclarationFlags,
        name: String,
        is_tuple: bool,
        generic_arguments: Option<NodeList>,
        children: NodeList,
    });
validator!(Interface {
        flags: DeclarationFlags,
        name: String,
        generic_arguments: Option<NodeList>,
        children: NodeList
    });

validator!(FunctionTraitType {
    arguments: NodeList,
    return_type: NodePtr
});

validator!(FunctionInterfaceType {
    arguments: NodeList,
    return_type: NodePtr
});

validator!(MutableFunctionTraitType {
    arguments: NodeList,
    return_type: NodePtr
});

validator!(MutableFunctionInterfaceType {
    arguments: NodeList,
    return_type: NodePtr
});

validator!(ModuleReference {
flags: DeclarationFlags,
name: String,
});

// This declares a module inline to a file
validator!(ModuleDeclaration {
flags: DeclarationFlags,
name: String,
children: NodeList
});

validator!(ImportTree {
name: String,
children: NodeList,
});

validator!(   StructureDestructureField{
        name: String,
        definition: NodePtr
    });

validator!(GenericArgument {
        name: String,
        argument_type: OptionalNode,
        default: OptionalNode
    });

pub fn v_program(children: NodeList) -> NodePtr {
    v_module(children)
}

pub fn v_single(child: NodePtr) -> NodePtr {
    v_program(vec![child])
}

pub const NO_STRING: Option<&str> = Option::None;

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