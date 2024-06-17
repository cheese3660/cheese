use std::collections::HashMap;
use std::fmt::{Display, Formatter};
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
impl Display for DeclarationFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.0 == 0 {
            return write!(f, "{}", "none".fg(Color::Blue))
        }
        let mut prefix = "";
        if self.contains(Self::inline) {
            write!(f, "{}{}", prefix, "inline".fg(Color::Blue))?;
            prefix = " ";
        }
        if self.contains(Self::external) {
            write!(f, "{}{}", prefix, "external".fg(Color::Blue))?;
            prefix = " ";
        }
        if self.contains(Self::export) {
            write!(f, "{}{}", prefix, "export".fg(Color::Blue))?;
            prefix = " ";
        }
        if self.contains(Self::comptime) {
            write!(f, "{}{}", prefix, "comptime".fg(Color::Blue))?;
            prefix = " ";
        }
        if self.contains(Self::public) {
            write!(f, "{}{}", prefix, "public".fg(Color::Blue))?;
            prefix = " ";
        }
        if self.contains(Self::private) {
            write!(f, "{}{}", prefix, "private".fg(Color::Blue))?;
            prefix = " ";
        }
        if self.contains(Self::mutable) {
            write!(f, "{}{}", prefix, "mutable".fg(Color::Blue))?;
            prefix = " ";
        }
        if self.contains(Self::entry) {
            write!(f, "{}{}", prefix, "entry".fg(Color::Blue))?;
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
        let pipes = vec![];
        self.data.display_depth(f, &pipes)
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
        operator: String,
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


fn write_pipes(f: &mut Formatter<'_>, pipes: &PipesList) -> std::fmt::Result {
    for (space,active) in pipes {
        if *active {
            write!(f, "│   ")?;
        } else {
            write!(f, "    ")?;
        }
        for i in 0..*space {
            write!(f, " ")?;
        }
    }
    Ok(())
}

fn write_split(f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "├─⇥ ")
}

fn write_end(f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "╰─⇥ ")
}

fn write_begin(f: &mut Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "╿")
}

fn write_empty(f: &mut Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "│")
}

fn with_empty(pipes: &PipesList) -> PipesList {
    let mut copy = pipes.clone();
    copy.push((0,false));
    copy
}

fn with_empty_count(pipes: &PipesList,count: usize) -> PipesList {
    let mut copy = pipes.clone();
    copy.push((count,false));
    copy
}

fn with_pipe(pipes: &PipesList) -> PipesList {
    let mut copy = pipes.clone();
    copy.push((0,true));
    copy
}

fn with_pipe_count(pipes: &PipesList,count: usize) -> PipesList {
    let mut copy = pipes.clone();
    copy.push((count,true));
    copy
}

fn write_list(f: &mut Formatter<'_>, pipes: &PipesList, list: &NodeList) -> std::fmt::Result {
    if list.len() > 0 {
        write_pipes(f, pipes)?;
        write_begin(f)?;
        let full = with_pipe(pipes);
        let empty = with_empty(pipes);
        let mut count = 0;
        for node in list {
            write_pipes(f, pipes)?;
            count += 1;
            if count == list.len() {
                write_end(f)?;
                node.data.display_depth(f, &empty)?;
            } else {
                write_split(f)?;
                node.data.display_depth(f, &full)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
            }
        }
    }
    Ok(())
}

fn write_dict(f: &mut Formatter<'_>, pipes: &PipesList, dict: &NodeDict) -> std::fmt::Result {
    if dict.len() > 0 {
        write_pipes(f, pipes)?;
        write_begin(f)?;
        let mut count = 0;
        for node in dict {
            write_pipes(f, pipes)?;
            count += 1;
            if count == dict.len() {
                let empty = with_empty_count(pipes,node.0.len());
                write_end(f)?;
                write!(f, "{}: ", node.0)?;
                node.1.data.display_depth(f, &empty)?;
            } else {
                let full = with_pipe_count(pipes,node.0.len());
                write_split(f)?;
                node.1.data.display_depth(f, &full)?;
                write!(f, "{}: ", node.0)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
            }
        }
    }
    Ok(())
}

fn write_unary(f: &mut Formatter, name: String, value: &NodePtr, pipes: &PipesList) -> std::fmt::Result {
    writeln!(f, "{}", name)?;
    write_pipes(f, pipes)?;
    write_begin(f)?;
    write_pipes(f, pipes)?;
    write_end(f)?;
    let empty = with_empty(pipes);
    value.data.display_depth(f, &empty)
}

fn write_binary(f: &mut Formatter, name: String, a: &NodePtr, b: &NodePtr, pipes: &PipesList) -> std::fmt::Result {
    writeln!(f, "{}", name)?;
    write_pipes(f, pipes)?;
    write_begin(f)?;
    write_pipes(f, pipes)?;
    write_split(f)?;
    let full = with_pipe(pipes);
    a.data.display_depth(f, &full)?;
    let empty = with_pipe(pipes);
    write_pipes(f, pipes)?;
    write_empty(f)?;
    write_pipes(f, pipes)?;
    write_end(f)?;
    b.data.display_depth(f, &empty)
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
            AstNodeData::FloatLiteral(f) => NodeBuilder::new_terminal(f,VALUE_COLOR),
            AstNodeData::ImaginaryLiteral(i) => NodeBuilder::new_terminal(format!("{i}I"),VALUE_COLOR),
            AstNodeData::IntegerLiteral(i) => NodeBuilder::new_terminal(i,VALUE_COLOR),
            AstNodeData::NameReference(name) => NodeBuilder::new_terminal(name,NAME_COLOR),
            AstNodeData::UnnamedBlock(children) => NodeBuilder::new("unnamed block",KEYWORD_COLOR).add_children(children).build(),
            AstNodeData::TupleLiteral(children) => NodeBuilder::new("tuple literal",OTHER_COLOR).add_children(children).build(),
            AstNodeData::ArrayLiteral(children) => NodeBuilder::new("array literal",OTHER_COLOR).add_children(children).build(),
            AstNodeData::EnumLiteral(name) => NodeBuilder::new("enum literal",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::BuiltinReference(name) => NodeBuilder::new("builtin reference",OTHER_COLOR).make_inline(name, METHOD_COLOR).build(),
            AstNodeData::CopyCapture(name) => NodeBuilder::new("copy capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::ReferenceCapture(name) => NodeBuilder::new("reference capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::ConstantReferenceCapture(name) => NodeBuilder::new("constant reference capture",OTHER_COLOR).make_inline(name, NAME_COLOR).build(),
            AstNodeData::ObjectLiteral(children) => NodeBuilder::new("object block",OTHER_COLOR).add_children(children).build(),
            AstNodeData::Block(children) => NodeBuilder::new("block",KEYWORD_COLOR).add_children(children).build(),
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
            AstNodeData::ImplicitArray { constant, subtype } => NodeBuilder::new(if constant { "implicit constant array type" } else {"implicit array type"},TYPE_COLOR).convert_child(subtype).build(),
            AstNodeData::Slice { constant, subtype } => NodeBuilder::new(if constant { "constant slice type" } else {"slice type"},TYPE_COLOR).convert_child(subtype).build(),
            AstNodeData::Reference { constant, subtype } => NodeBuilder::new(if constant { "constant reference type" } else {"reference type"},TYPE_COLOR).convert_child(subtype).build(),
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
            AstNodeData::NamedBlock { name, body } =>
                NodeBuilder::new("named block",KEYWORD_COLOR)
                    .make_field("name",name,METHOD_COLOR)
                    .list_field("body",body)
                    .build(),
            AstNodeData::NamedBreak { name, value } =>
                NodeBuilder::new("break(...)",KEYWORD_COLOR)
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
                    None => builder.make_field("store into", "_",NAME_COLOR),
                    Some(store) => builder.convert_field("store into",store),
                }.list_field("matches",matches).build()
            }
            AstNodeData::MatchEnumStructure { enum_identifier, children } => {
                let mut builder = NodeBuilder::new("enum structure match",KEYWORD_COLOR);
                builder.make_field("id",enum_identifier,NAME_COLOR);
                let mut sub_builder = NodeBuilder::new_unnamed();
                for (name, field) in children {
                    sub_builder.convert_field(name,field)
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
                    builder.convert_field(name,field)
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
            AstNodeData::Enum { containing_type, children } => match containing_type {
                None => NodeBuilder::new("enum",TYPE_COLOR).add_children(children).build(),
                Some(containing_type) => NodeBuilder::new("enum",TYPE_COLOR).convert_field("containing type",containing_type).list_field("values",children).build(),
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
                let mut builder = NodeBuilder::new(if is_tuple { "tuple" } else { "structure" }, TYPE_COLOR);
                if interfaces.len() == 0 {
                    builder.add_children(children);
                } else {
                    builder.list_field("interfaces", interfaces);
                    builder.list_field("children", children);
                }
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
                builder.make_field("operator",operator, METHOD_COLOR).make_field("flags",format!("{flags}"),KEYWORD_COLOR);
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
            AstNodeData::AnonymousFunction { flags, arguments, return_type, body } => {
                let mut builder = NodeBuilder::new("anonymous function", KEYWORD_COLOR);
                builder.make_field("flags",format!("{flags}"),KEYWORD_COLOR).list_field("arguments",arguments);
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
                for (name, store) in fields {
                    builder.convert_field(name,store);
                }
                builder.build()
            },
            AstNodeData::TupleDestructure(fields) => NodeBuilder::new("tuple destructure", OTHER_COLOR).add_children(fields).build(),
            AstNodeData::ArrayDestructure(fields) => NodeBuilder::new("array destructure", OTHER_COLOR).add_children(fields).build(),
            AstNodeData::SliceDestructure(fields) => NodeBuilder::new("slice destructure", OTHER_COLOR).add_children(fields).build(),
            AstNodeData::Destructure { structure, value } => NodeBuilder::new("destructure",KEYWORD_COLOR).convert_field("structure", structure).convert_field("value", value).build(),
            AstNodeData::Interface { interfaces, children, dynamic } => {
                let mut builder = NodeBuilder::new(if dynamic { "dynamic interface" } else { "interface" }, TYPE_COLOR);
                if interfaces.len() == 0 {
                    builder.add_children(children);
                } else {
                    builder.list_field("interfaces", interfaces);
                    builder.list_field("children", children);
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
        }
    }
}

/*
impl AstNodeData {
    fn display_depth(&self, f: &mut Formatter<'_>, pipes: &PipesList) -> std::fmt::Result {
        match self {
            AstNodeData::Bool => writeln!(f, "{}", "bool".fg(Color::Red)),
            AstNodeData::SignedSize => writeln!(f, "{}", "isize".fg(Color::Red)),
            AstNodeData::UnsignedSize => writeln!(f, "{}", "usize".fg(Color::Red)),
            AstNodeData::Float32 => writeln!(f, "{}", "f32".fg(Color::Red)),
            AstNodeData::Float64 => writeln!(f, "{}", "f64".fg(Color::Red)),
            AstNodeData::Complex32 => writeln!(f, "{}", "c32".fg(Color::Red)),
            AstNodeData::Complex64 => writeln!(f, "{}", "c64".fg(Color::Red)),
            AstNodeData::Opaque => writeln!(f, "{}", "opaque".fg(Color::Red)),
            AstNodeData::Void => writeln!(f, "{}", "void".fg(Color::Red)),
            AstNodeData::CompileTimeFloat => writeln!(f, "{}", "comptime_float".fg(Color::Red)),
            AstNodeData::CompileTimeComplex => writeln!(f, "{}", "comptime_complex".fg(Color::Red)),
            AstNodeData::CompileTimeString => writeln!(f, "{}", "comptime_string".fg(Color::Red)),
            AstNodeData::CompileTimeInteger => writeln!(f, "{}", "comptime_integer".fg(Color::Red)),
            AstNodeData::Type => writeln!(f, "{}", "type".fg(Color::Red)),
            AstNodeData::NoReturn => writeln!(f, "{}", "false".fg(Color::Red)),
            AstNodeData::True => writeln!(f, "{}", "true".fg(Color::Magenta)),
            AstNodeData::False => writeln!(f, "{}", "false".fg(Color::Magenta)),
            AstNodeData::None => writeln!(f, "{}", "none".fg(Color::Magenta)),
            AstNodeData::Underscore => writeln!(f, "{}", '_'.fg(Color::Cyan)),
            AstNodeData::Continue => writeln!(f, "{}", "continue".fg(Color::Blue)),
            AstNodeData::EmptyBreak => writeln!(f, "{}", "break".fg(Color::Blue)),
            AstNodeData::EmptyReturn => writeln!(f, "{}", "return".fg(Color::Blue)),
            AstNodeData::SelfValue => writeln!(f, "{}", "self".fg(Color::Cyan)),
            AstNodeData::ConstSelfValue => writeln!(f, "{}", "~self".fg(Color::Cyan)),
            AstNodeData::SelfType => writeln!(f, "{}", "Self".fg(Color::Red)),
            AstNodeData::ConstReferenceImplicitCapture => writeln!(f, "{}","implicit constant capture".fg(Color::Yellow)),
            AstNodeData::ReferenceImplicitCapture => writeln!(f, "{}", "implicit reference capture".fg(Color::Yellow)),
            AstNodeData::CopyImplicitCapture => writeln!(f, "{}", "implicit copy capture".fg(Color::Yellow)),
            AstNodeData::MatchAll => writeln!(f, "{}", "match all".fg(Color::Yellow)),
            AstNodeData::NonExhaustive => writeln!(f, "{}", "non exhaustive match".fg(Color::Yellow)),
            AstNodeData::UnknownSize => writeln!(f, "{}", "unknown".fg(Color::Magenta)),
            AstNodeData::InferredSize => writeln!(f, "{}", "inferred".fg(Color::Magenta)),
            AstNodeData::SignedIntegerType(size) => writeln!(f, "{}{}", 'i'.fg(Color::Red), size.fg(Color::Red)),
            AstNodeData::UnsignedIntegerType(size) => writeln!(f, "{}{}", 'u'.fg(Color::Red), size.fg(Color::Red)),
            AstNodeData::StringLiteral(s) => writeln!(f, "{}", s.escape_with_quotes("\"").fg(Color::Red)),
            AstNodeData::FloatLiteral(flt) => writeln!(f, "{}", flt.fg(Color::Magenta)),
            AstNodeData::IntegerLiteral(i) => writeln!(f, "{}", i.fg(Color::Magenta)),
            AstNodeData::ImaginaryLiteral(flt) => writeln!(f, "{}{}", flt.fg(Color::Magenta), 'I'.fg(Color::Magenta)),
            AstNodeData::NameReference(name) => writeln!(f, "{}", name.fg(Color::Cyan)),
            AstNodeData::UnnamedBlock(children) => {
                writeln!(f, "{}","unnamed block".fg(Color::Yellow))?;
                write_list(f, pipes, children)
            }
            AstNodeData::TupleLiteral(children) => {
                writeln!(f, "{}","tuple literal".fg(Color::Yellow))?;
                write_list(f, pipes, children)
            }
            AstNodeData::ArrayLiteral(children) => {
                writeln!(f, "{}","array literal".fg(Color::Yellow))?;
                write_list(f, pipes, children)
            }
            AstNodeData::EnumLiteral(lit) => writeln!(f, "{}: {}", "enum literal".fg(Color::Yellow), lit.fg(Color::Cyan)),
            AstNodeData::BuiltinReference(reference) => writeln!(f, "{}: {}","builtin reference".fg(Color::Yellow), reference.fg(Color::BrightYellow)),
            AstNodeData::CopyCapture(var) => writeln!(f, "{}: {}","copy capture".fg(Color::Yellow), var.fg(Color::Cyan)),
            AstNodeData::ReferenceCapture(var) => writeln!(f, "{}: {}", "reference capture".fg(Color::Yellow), var.fg(Color::Cyan)),
            AstNodeData::ConstantReferenceCapture(var) => writeln!(f, "{}: {}","constant reference capture".fg(Color::Yellow), var.fg(Color::Cyan)),
            AstNodeData::ObjectLiteral(children) => {
                writeln!(f, "{}", "object literal".fg(Color::Yellow))?;
                write_list(f, pipes, children)
            }
            AstNodeData::Block(children) => {
                writeln!(f, "{}", "block".fg(Color::Yellow))?;
                write_list(f, pipes, children)
            }
            AstNodeData::Break(value) =>
                write_unary(f, format!("{}", "break".fg(Color::Blue)), value, pipes),
            AstNodeData::Return(value) =>
                write_unary(f, format!("{}", "return".fg(Color::Blue)), value, pipes),
            AstNodeData::ImplicitResult(value) =>
                write_unary(f, format!("{}", "implicit".fg(Color::Blue)), value, pipes),
            AstNodeData::Yield(value) =>
                write_unary(f, format!("{}", "yield".fg(Color::Blue)), value, pipes),
            AstNodeData::Not(value) =>
                write_unary(f, format!("{}", "not".fg(Color::Blue)), value, pipes),
            AstNodeData::UnaryMinus(value) =>
                write_unary(f, format!("{}", "unary -".fg(Color::Blue)), value, pipes),
            AstNodeData::UnaryPlus(value) =>
                write_unary(f, format!("{}", "unary +".fg(Color::Blue)), value, pipes),
            AstNodeData::Dereference(value) =>
                write_unary(f, format!("{}", "$".fg(Color::Blue)), value, pipes),
            AstNodeData::AddressOf(value) =>
                write_unary(f, format!("{}", "&".fg(Color::Blue)), value, pipes),
            AstNodeData::Concept(value) =>
                write_unary(f, format!("{}", "concept".fg(Color::Blue)), value, pipes),
            AstNodeData::Loop(value) =>
                write_unary(f, format!("{}", "loop".fg(Color::Blue)), value, pipes),
            AstNodeData::FilterTransformation(value) =>
                write_unary(f, format!("{}", "?".fg(Color::Blue)), value, pipes),
            AstNodeData::MapTransformation(value) =>
                write_unary(f, format!("{}", "|".fg(Color::Blue)), value, pipes),
            AstNodeData::Comptime(value) =>
                write_unary(f, format!("{}", "comptime".fg(Color::Blue)), value, pipes),
            AstNodeData::ImplicitArray {
                constant,
                subtype
            } =>
                write_unary(f, format!("{}",(if *constant {
                    "constant implicit array type type"
                } else {
                    "implicit array type"
                }).fg(Color::Red)), subtype, pipes),
            AstNodeData::Slice {
                constant,
                subtype
            } =>
                write_unary(f, format!("{}",(if *constant {
                    "constant slice type"
                } else {
                    "slice type"
                }).fg(Color::Red)), subtype, pipes),
            AstNodeData::Reference {
                constant,
                subtype
            } =>
                write_unary(f, format!("{}",(if *constant {
                    "constant reference type"
                } else {
                    "reference type"
                }).fg(Color::Red)), subtype, pipes),
            AstNodeData::TupleCall {
                functional,
                args
            } => {
                let full_value = with_pipe_count(pipes,7);
                let empty_value = with_empty_count(pipes,7);
                let empty = with_empty(pipes);
                writeln!(f, "{}", "()".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                if args.is_empty() {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                write!(f, "value: ")?;
                functional.data.display_depth(f, if args.is_empty() { &empty_value } else { &full_value })?;
                if !args.is_empty() {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    writeln!(f, "arguments")?;
                    write_list(f, &empty, args)?;
                }
                Ok(())
            }
            AstNodeData::ArrayCall {
                functional,
                args
            } => {
                let full_value = with_pipe_count(pipes,7);
                let empty_value = with_empty_count(pipes,7);
                let empty = with_empty(pipes);
                writeln!(f, "{}", "[]".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                if args.is_empty() {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                write!(f, "value: ")?;
                functional.data.display_depth(f, if args.is_empty() { &empty_value } else { &full_value })?;
                if !args.is_empty() {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    writeln!(f, "arguments")?;
                    write_list(f, &empty, args)?;
                }
                Ok(())
            }
            AstNodeData::NamedBlock {
                name,
                body
            } => {
                writeln!(f, "{}: {}", "block".fg(Color::Yellow), name.fg(Color::BrightYellow))?;
                write_list(f, pipes, body)
            }
            AstNodeData::NamedBreak {
                name,
                value
            } =>
                write_unary(f, format!("{}: {}", "yield".fg(Color::Blue), name.fg(Color::BrightYellow)), value, pipes),
            AstNodeData::ObjectCall {
                functional,
                args
            } => {
                let full_value = with_pipe_count(pipes,7);
                let empty_value = with_empty_count(pipes,7);
                let empty = with_empty(pipes);
                writeln!(f, "{}", "{}".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                if args.is_empty() {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                write!(f, "value: ")?;
                functional.data.display_depth(f, if args.is_empty() { &empty_value } else { &full_value })?;
                if !args.is_empty() {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    writeln!(f, "arguments")?;
                    write_list(f, &empty, args)?;
                }
                Ok(())
            }
            AstNodeData::ErrorNode {
                message,
                ..
            } => writeln!(f, "{}: {}", "error".fg(Color::BrightRed), message),
            AstNodeData::Match {
                value,
                arms
            } => {
                let full_value = with_pipe_count(pipes,7);
                let empty_value = with_empty_count(pipes,7);
                let empty = with_empty(pipes);
                writeln!(f, "{}", "match".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                if arms.is_empty() {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                write!(f, "value: ")?;
                value.data.display_depth(f, if arms.is_empty() { &empty_value } else { &full_value })?;
                if !arms.is_empty() {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    writeln!(f, "arms")?;
                    write_list(f, &empty, arms)?;
                }
                Ok(())
            }
            AstNodeData::MatchRange {
                begin,
                end
            } => write_binary(f, format!("{}","match range".fg(Color::Yellow)), begin, end, pipes),
            AstNodeData::DestructuringMatchArm {
                matches,
                store
            } => {
                let full_store = with_pipe_count(pipes,12);
                let empty_store = with_empty_count(pipes,12);
                let empty = with_empty(pipes);
                writeln!(f, "{}", "destructuring match".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                if matches.is_empty() {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                write!(f, "store into: ")?;
                if let Some(s) = store {
                    s.data.display_depth(f, if matches.is_empty() { &empty_store } else { &full_store })?;
                } else {
                    writeln!(f, "{}", "_".fg(Color::Cyan))?;
                }
                if !matches.is_empty() {
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    writeln!(f, "matches")?;
                    write_list(f, &empty, matches)?;
                }
                Ok(())
            }
            AstNodeData::MatchEnumStructure {
                enum_identifier,
                children
            } => {
                let empty = with_empty(pipes);
                writeln!(f, "{}", "match enum structure".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                if children.is_empty() {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                writeln!(f, "enum: {}", enum_identifier.fg(Color::Cyan))?;
                if !children.is_empty() {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    writeln!(f, "fields")?;
                    write_dict(f, &empty, children)?;
                }
                Ok(())
            }
            AstNodeData::MatchEnumTuple {
                enum_identifier,
                children
            } => {
                let empty = with_empty(pipes);
                writeln!(f, "{}", "match enum tuple".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                if children.is_empty() {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                writeln!(f, "enum: {}", enum_identifier.fg(Color::Cyan))?;
                if !children.is_empty() {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    writeln!(f, "fields")?;
                    write_list(f, &empty, children)?;
                }
                Ok(())
            }
            AstNodeData::MatchValue(value) => write_unary(f, format!("{}", "match value".fg(Color::Blue)), value, pipes),
            AstNodeData::MatchConstraint(constraint) => write_unary(f, format!("{}", "match constraint".fg(Color::Blue)), constraint, pipes),
            AstNodeData::DestructuringMatchStructure(structure) => {
                writeln!(f, "{}", "match structure".fg(Color::Blue))?;
                write_dict(f, pipes, structure)
            }
            AstNodeData::DestructuringMatchTuple(tuple) => {
                writeln!(f, "{}","match tuple".fg(Color::Blue))?;
                write_list(f, pipes, tuple)
            }
            AstNodeData::DestructuringMatchArray(array) => {
                writeln!(f, "{}","match array".fg(Color::Blue))?;
                write_list(f, pipes, array)
            }
            AstNodeData::Enum {
                containing_type,
                children
            } => {
                writeln!(f, "{}", "enum".fg(Color::Red))?;
                match containing_type {
                    None => write_list(f, pipes, children),
                    Some(c_type) => {
                        let full_containing = with_pipe_count(pipes,17);
                        let empty_containing = with_empty_count(pipes,17);
                        let empty = with_empty(pipes);
                        write_pipes(f, pipes)?;
                        write_begin(f)?;
                        write_pipes(f, pipes)?;
                        if children.is_empty() {
                            write_end(f)?;
                        } else {
                            write_split(f)?;
                        }
                        write!(f, "containing type: ")?;
                        c_type.data.display_depth(f, if children.is_empty() { &empty_containing } else { &full_containing })?;
                        if !children.is_empty() {
                            write_pipes(f, pipes)?;
                            write_empty(f)?;
                            write_pipes(f, pipes)?;
                            write_end(f)?;
                            writeln!(f, "children")?;
                            write_list(f, &empty, children)?;
                        }
                        Ok(())
                    }
                }
            }
            AstNodeData::FieldLiteral {
                name,
                value
            } => write_unary(f, format!("{}: {}", "field literal".fg(Color::Yellow), name.fg(Color::Cyan)), value, pipes),
            AstNodeData::Subscription {
                lhs,
                rhs
            } => write_binary(f, format!("{}",".".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Multiplication {
                lhs,
                rhs
            } => write_binary(f, format!("{}","*".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Division {
                lhs,
                rhs
            } => write_binary(f, format!("{}","/".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Modulus {
                lhs,
                rhs
            } => write_binary(f, format!("{}","%".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Addition {
                lhs,
                rhs
            } => write_binary(f, format!("{}","+".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Subtraction {
                lhs,
                rhs
            } => write_binary(f, format!("{}","-".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::LeftShift {
                lhs,
                rhs
            } => write_binary(f, format!("{}","<<".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::RightShift {
                lhs,
                rhs
            } => write_binary(f, format!("{}",">>".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::LesserThan {
                lhs,
                rhs
            } => write_binary(f, format!("{}","<".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::GreaterThan {
                lhs,
                rhs
            } => write_binary(f, format!("{}",">".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::LesserEqual {
                lhs,
                rhs
            } => write_binary(f, format!("{}","<=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::GreaterEqual {
                lhs,
                rhs
            } => write_binary(f, format!("{}",">=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::EqualTo {
                lhs,
                rhs
            } => write_binary(f, format!("{}","==".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::NotEqualTo {
                lhs,
                rhs
            } => write_binary(f, format!("{}","!=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::And {
                lhs,
                rhs
            } => write_binary(f, format!("{}", "and".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Or {
                lhs,
                rhs
            } => write_binary(f, format!("{}", "or".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Xor {
                lhs,
                rhs
            } => write_binary(f, format!("{}", "xor".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Combine {
                lhs,
                rhs
            } => write_binary(f, format!("{}","&".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Reassign {
                lhs,
                rhs
            } => write_binary(f, format!("{}",":=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::Assign {
                lhs,
                rhs
            } => write_binary(f, format!("{}","=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::AddAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}","+=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::SubtractAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}","-=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::MultiplyAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}","*=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::DivideAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}","/=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::ModulateAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}","%=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::ShiftLeftAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}","<<=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::ShiftRightAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}",">>=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::AndAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}", "and=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::OrAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}", "or=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::XorAssign {
                lhs,
                rhs
            } => write_binary(f, format!("{}", "xor=".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::IsType {
                lhs,
                rhs
            } => write_binary(f, format!("{}", "is".fg(Color::Blue)), lhs, rhs, pipes),
            AstNodeData::DynamicCast {
                lhs,
                to
            } => write_binary(f, format!("{}","@*".fg(Color::Blue)), lhs, to, pipes),
            AstNodeData::Cast {
                lhs,
                to
            } => write_binary(f, format!("{}","@".fg(Color::Blue)), lhs, to, pipes),
            AstNodeData::Range {
                begin,
                end
            } => write_binary(f, format!("{}","range".fg(Color::Blue)), begin, end, pipes),
            AstNodeData::TypeDeclaration {
                flags,
                name,
                generic_arguments,
                alias
            } => {
                writeln!(f, "{}", "type".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "name: {}", name.fg(Color::Red))?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "flags:{}", flags)?;
                if let Some(args) = generic_arguments {
                    let full = with_pipe(pipes);
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    writeln!(f, "generic arguments")?;
                    write_list(f, &full, args)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                let empty = with_empty_count(pipes,7);
                write!(f, "alias: ")?;
                alias.data.display_depth(f, &empty)
            }
            AstNodeData::Field {
                flags, name, field_type
            } => {
                writeln!(f, "{}", "field".fg(Color::Yellow))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                if let Some(name) = name {
                    writeln!(f, "name: {}", name.fg(Color::Cyan))?;
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                }
                writeln!(f, "flags:{}", flags)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                let empty = with_empty_count(pipes,6);
                write!(f, "type: ")?;
                field_type.data.display_depth(f, &empty)
            }
            AstNodeData::Argument {
                name, argument_type
            } => {
                match name {
                    Some(name) => {
                        writeln!(f, "{}","argument".fg(Color::Yellow))?;
                        write_pipes(f, pipes)?;
                        write_begin(f)?;
                        write_pipes(f, pipes)?;
                        write_split(f)?;
                        writeln!(f, "name: {}", name.fg(Color::Cyan))?;
                        write_pipes(f, pipes)?;
                        write_empty(f)?;
                        write_pipes(f, pipes)?;
                        write_end(f)?;
                        let empty = with_empty_count(pipes,6);
                        write!(f, "type: ")?;
                        argument_type.data.display_depth(f, &empty)
                    }
                    None => write_unary(f, "argument".to_string(), argument_type, pipes)
                }
            }
            AstNodeData::Import {
                path,
                name
            } => {
                writeln!(f, "{}", "import".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "path: {}", path.fg(Color::Green))?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                writeln!(f, "name: {}", name.fg(Color::Red))
            }
            AstNodeData::Structure {
                interfaces,
                children,
                is_tuple
            } => {
                writeln!(f, "{}", if *is_tuple {
                    "tuple".fg(Color::Red)
                } else {
                    "structure".fg(Color::Red)
                })?;
                if interfaces.is_empty() {
                    write_list(f, pipes, children)
                } else {
                    write_pipes(f, pipes)?;
                    write_begin(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    let full = with_pipe(pipes);
                    let empty = with_empty(pipes);
                    writeln!(f, "interfaces")?;
                    write_list(f, &full, interfaces)?;
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    write_list(f, &empty, children)
                }
            }
            AstNodeData::FunctionPrototype {
                flags, name, arguments, return_type
            } => {
                let full = with_pipe(pipes);
                let empty = with_empty(pipes);
                writeln!(f, "{}", "fn prototype".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "name: {}", name.fg(Color::BrightYellow))?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "flags:{}", flags)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                if let None = return_type {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                writeln!(f, "arguments")?;
                write_list(f, if let None = return_type { &empty } else { &full }, arguments)?;
                if let Some(return_type) = return_type {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    let return_type_empty = with_empty_count(pipes,13);
                    write!(f, "return type: ")?;
                    return_type.data.display_depth(f, &return_type_empty)?;
                }
                Ok(())
            }
            AstNodeData::FunctionImport {
                flags, name, arguments, return_type, import_name: library_name
            } => {
                let full = with_pipe(pipes);
                let empty = with_empty(pipes);
                writeln!(f, "{}", "fn import".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "name: {}", name.fg(Color::BrightYellow))?;
                // now we need the import name
                if let Some(import_name) = library_name {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    let import_full = with_pipe_count(pipes,14);
                    write!(f, "library name: ")?;
                    import_name.data.display_depth(f, &import_full)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "flags:{}", flags)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                if let None = return_type {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                writeln!(f, "arguments")?;
                write_list(f, if let None = return_type { &empty } else { &full }, arguments)?;

                if let Some(return_type) = return_type {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    let return_type_empty = with_empty_count(pipes,13);
                    write!(f, "return type: ")?;
                    return_type.data.display_depth(f, &return_type_empty)?;
                }
                Ok(())
            }
            AstNodeData::Function { flags, name, generic_arguments, arguments, return_type, body } => {
                let full = with_pipe(pipes);
                writeln!(f, "{}", "fn".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "name: {}", name.fg(Color::BrightYellow))?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "flags:{}", flags)?;
                if let Some(generic_arguments) = generic_arguments {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    writeln!(f, "generic arguments")?;
                    write_list(f, &full, generic_arguments)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "arguments")?;
                write_list(f, &full, arguments)?;
                if let Some(return_type) = return_type {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    write!(f, "return type: ")?;
                    let return_type_full = with_pipe_count(pipes,13);
                    return_type.data.display_depth(f, &return_type_full)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                let body_empty = with_empty_count(pipes,6);
                write!(f, "body: ")?;
                body.data.display_depth(f, &body_empty)
            }
            AstNodeData::Operator {
                flags, operator, generic_arguments, arguments, return_type, body
            } => {
                let full = with_pipe(pipes);
                writeln!(f, "{}", "operator".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "operator: {}", operator.fg(Color::BrightYellow))?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "flags:{}", flags)?;
                if let Some(generic_arguments) = generic_arguments {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    writeln!(f, "generic arguments")?;
                    write_list(f, &full, generic_arguments)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "arguments")?;
                write_list(f, &full, arguments)?;
                if let Some(return_type) = return_type {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    write!(f, "return type: ")?;
                    let return_type_full = with_pipe_count(pipes,13);
                    return_type.data.display_depth(f, &return_type_full)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                let body_empty = with_empty_count(pipes,6);
                write!(f, "body: ")?;
                body.data.display_depth(f, &body_empty)
            }
            AstNodeData::VariableDeclaration {
                definition, value
            } => write_binary(f, format!("{}","variable declaration".fg(Color::Yellow)), definition, value, pipes),
            AstNodeData::VariableDefinition { flags, name, variable_type } => {
                writeln!(f, "{}","variable definition".fg(Color::Yellow))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "name: {}", name.fg(Color::Cyan))?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                match variable_type {
                    None => {
                        write_end(f)?;
                        writeln!(f, "flags:{}", flags)
                    }
                    Some(variable_type) => {
                        let empty = with_empty_count(pipes,6);
                        write_split(f)?;
                        writeln!(f, "flags:{}", flags)?;
                        write_pipes(f, pipes)?;
                        write_empty(f)?;
                        write_pipes(f, pipes)?;
                        write_end(f)?;
                        write!(f, "type: ")?;
                        variable_type.data.display_depth(f, &empty)
                    }
                }
            }
            AstNodeData::Closure {
                arguments, captures, return_type, body
            } => {
                let full = with_pipe(pipes);
                writeln!(f, "{}","closure".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "arguments")?;
                write_list(f, &full, arguments)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "captures")?;
                write_list(f, &full, captures)?;
                if let Some(return_type) = return_type {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    let return_type_full = with_pipe_count(pipes,13);
                    write!(f, "return type: ")?;
                    return_type.data.display_depth(f, &return_type_full)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                write!(f, "body: ")?;
                let body_empty = with_empty_count(pipes,6);
                body.data.display_depth(f, &body_empty)
            }
            AstNodeData::AnonymousFunction {
                flags, arguments, return_type, body
            } => {
                let full = with_pipe(pipes);
                writeln!(f, "{}", "anonymous fn".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "flags:{}", flags)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "arguments")?;
                write_list(f, &full, arguments)?;
                if let Some(return_type) = return_type {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    let return_type_full = with_pipe_count(pipes,13);
                    write!(f, "return type: ")?;
                    return_type.data.display_depth(f, &return_type_full)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                write!(f, "body: ")?;
                let body_empty = with_empty_count(pipes,6);
                body.data.display_depth(f, &body_empty)
            }
            AstNodeData::FunctionType { flags, arguments, return_type } => {
                let full = with_pipe(pipes);
                let empty = with_empty(pipes);
                writeln!(f, "{}", "fn type".fg(Color::Red))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "flags:{}", flags)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "arguments")?;
                write_list(f, &full, arguments)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                write!(f, "return type: ")?;
                return_type.data.display_depth(f, &empty)
            }
            AstNodeData::StructureDestructure(nodes) => {
                writeln!(f, "{}", "structure destructure".fg(Color::Yellow))?;
                write_dict(f, pipes, nodes)
            }
            AstNodeData::TupleDestructure(nodes) => {
                writeln!(f, "{}","tuple destructure".fg(Color::Yellow))?;
                write_list(f, pipes, nodes)
            }
            AstNodeData::ArrayDestructure(nodes) => {
                writeln!(f, "{}","array destructure".fg(Color::Yellow))?;
                write_list(f, pipes, nodes)
            }
            AstNodeData::SliceDestructure(nodes) => {
                writeln!(f, "{}","slice destructure".fg(Color::Yellow))?;
                write_list(f, pipes, nodes)
            }
            AstNodeData::Destructure { structure, value } => write_binary(f, format!("{}","destructure".fg(Color::Yellow)), structure, value, pipes),
            AstNodeData::Interface { interfaces, children, dynamic } => {
                writeln!(f, "{}", if *dynamic {
                    "dynamic interface".fg(Color::Red)
                } else {
                    "interface".fg(Color::Red)
                })?;
                if interfaces.is_empty() {
                    write_list(f, pipes, children)
                } else {
                    write_pipes(f, pipes)?;
                    write_begin(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    let full = with_pipe(pipes);
                    let empty = with_empty(pipes);
                    writeln!(f, "interfaces")?;
                    write_list(f, &full, interfaces)?;
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    write_list(f, &empty, children)
                }
            }
            AstNodeData::If {
                condition, unwrap, body, else_statement
            } => {
                writeln!(f, "{}", "if".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                write!(f, "condition: ")?;
                let condition_full = with_pipe_count(pipes,11);
                condition.data.display_depth(f, &condition_full)?;
                if let Some(unwrap) = unwrap {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    write!(f, "unwrap: ")?;
                    let unwrap_full = with_pipe_count(pipes,8);
                    unwrap.data.display_depth(f, &unwrap_full)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                if let None = else_statement {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                write!(f, "body: ")?;
                let body_pipes = if let None = else_statement {with_empty_count} else {with_pipe_count}(pipes,6);
                body.data.display_depth(f, &body_pipes)?;
                if let Some(else_statement) = else_statement {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    let else_pipes = with_empty_count(pipes,6);
                    write!(f, "else: ")?;
                    else_statement.data.display_depth(f, &else_pipes)?;
                }
                Ok(())
            }
            AstNodeData::ComptimeIf { condition, body, else_statement } => {
                writeln!(f, "{}", "comptime if".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                write!(f, "condition: ")?;
                let condition_full = with_pipe_count(pipes,11);
                condition.data.display_depth(f, &condition_full)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                if let None = else_statement {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                write!(f, "body: ")?;
                let body_pipes = if let None = else_statement {with_empty_count} else {with_pipe_count}(pipes,6);
                body.data.display_depth(f, &body_pipes)?;
                if let Some(else_statement) = else_statement {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    let else_pipes = with_empty_count(pipes,6);
                    write!(f, "else: ")?;
                    else_statement.data.display_depth(f, &else_pipes)?;
                }
                Ok(())
            }
            AstNodeData::While { condition, body, else_statement } => {
                writeln!(f, "{}", "while".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                write!(f, "condition: ")?;
                let condition_full = with_pipe_count(pipes,11);
                condition.data.display_depth(f, &condition_full)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                if let None = else_statement {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                write!(f, "body: ")?;
                let body_pipes = if let None = else_statement {with_empty_count} else {with_pipe_count}(pipes,6);
                body.data.display_depth(f, &body_pipes)?;
                if let Some(else_statement) = else_statement {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    let else_pipes = with_empty_count(pipes,6);
                    write!(f, "else: ")?;
                    else_statement.data.display_depth(f, &else_pipes)?;
                }
                Ok(())
            }
            AstNodeData::For { capture, index_capture, iterable, transformations, body, else_statement } => {
                let full = with_pipe(pipes);
                writeln!(f, "{}", "for".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                write!(f, "capture: ")?;
                let capture_full = with_pipe_count(pipes,9);
                capture.data.display_depth(f, &capture_full)?;
                if let Some(index) = index_capture {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    write!(f, "index: ")?;
                    let index_full = with_pipe_count(pipes,7);
                    index.data.display_depth(f, &index_full)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                write!(f, "iterable: ")?;
                let iterable_full = with_pipe_count(pipes,10);
                iterable.data.display_depth(f, &full)?;
                if !transformations.is_empty() {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    writeln!(f, "transformations")?;
                    write_list(f,&full,transformations)?;
                }
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                if let None = else_statement {
                    write_end(f)?;
                } else {
                    write_split(f)?;
                }
                write!(f, "body: ")?;
                let body_pipes = if let None = else_statement {with_empty_count} else {with_pipe_count}(pipes,6);
                body.data.display_depth(f, &body_pipes)?;
                if let Some(else_statement) = else_statement {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    let else_pipes = with_empty_count(pipes,6);
                    write!(f, "else: ")?;
                    else_statement.data.display_depth(f, &else_pipes)?;
                }
                Ok(())
            }
            AstNodeData::MatchArm { matches, store, body } => {
                let full = with_pipe(pipes);
                writeln!(f, "{}","match arm".fg(Color::Yellow))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                if let Some(store) = store {
                    write!(f, "store: ")?;
                    let store_pipes = with_pipe_count(pipes, 7);
                    store.data.display_depth(f,&store_pipes)?;
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                }
                writeln!(f, "matches")?;
                write_list(f,&full,matches)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                write!(f, "body: ")?;
                let body_pipes = with_empty_count(pipes,6);
                body.data.display_depth(f,&body_pipes)
            }
            AstNodeData::EnumMember { name, tuple, children, value } => {
                let empty = with_empty(pipes);
                writeln!(f, "{}","enum member".fg(Color::Yellow))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "name: {}", name.fg(Color::Cyan))?;
                if !children.is_empty() {
                    let full = with_pipe(pipes);
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_split(f)?;
                    writeln!(f, "tuple: {}", tuple.fg(Color::BrightMagenta))?;
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    if let None = value {
                        write_end(f)?;
                    } else {
                        write_split(f)?;
                    }
                    writeln!(f, "children")?;
                    write_list(f,if let None = value { &empty } else {&full}, children)?;
                }
                if let Some(value) = value {
                    write_pipes(f, pipes)?;
                    write_empty(f)?;
                    write_pipes(f, pipes)?;
                    write_end(f)?;
                    write!(f,"value: ")?;
                    let value_pipes = with_empty_count(pipes,7);
                    value.data.display_depth(f,&value_pipes)?;
                }
                Ok(())
            }
            AstNodeData::ArrayType { constant, dimensions, child } => {
                let empty = with_empty(pipes);
                let full = with_pipe(pipes);
                writeln!(f, "{}","array type".fg(Color::Red))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                writeln!(f, "constant: {}", constant.fg(Color::BrightMagenta))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_empty(f)?;
                write_split(f)?;
                writeln!(f, "dimensions")?;
                write_list(f, &full, dimensions)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                child.data.display_depth(f,&empty)
            }
            AstNodeData::TypeMemberReference { referee, member } => {
                writeln!(f, "{}","::".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                write!(f, "type: ")?;
                let type_pipes = with_pipe_count(pipes,6);
                referee.data.display_depth(f,&type_pipes)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                writeln!(f, "member: {}", member.fg(Color::Cyan))
            }
            AstNodeData::GenericInstanceReference { referee, generic_args } => {
                let empty = with_empty(pipes);
                writeln!(f, "{}","::<>".fg(Color::Blue))?;
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f, pipes)?;
                write_split(f)?;
                write!(f, "referee: ")?;
                let referee_pipes = with_pipe_count(pipes,9);
                referee.data.display_depth(f,&referee_pipes)?;
                write_pipes(f, pipes)?;
                write_empty(f)?;
                write_pipes(f, pipes)?;
                write_end(f)?;
                writeln!(f, "arguments")?;
                write_list(f,&empty,generic_args)
            }
        }
    }
}
*/