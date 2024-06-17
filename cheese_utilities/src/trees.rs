// This is an entire tree drawing module, using a Node

use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::fmt::{Display, Formatter};
use ariadne::{Color,Fmt};
use crate::trees::DisplayNodeData::{Dictionary, Inline, List, Terminal};


#[derive(Debug, Clone)]
pub struct DisplayNode {
    name: Option<DisplayNodeName>,
    data: DisplayNodeData,
}

#[derive(Debug, Clone)]
pub struct DisplayNodeName {
    name: String,
    color: Color
}

#[derive(Debug,Clone)]
pub enum DisplayNodeData {
    Terminal,
    Inline(Box<DisplayNode>),
    List(Vec<Box<DisplayNode>>),
    Dictionary(Vec<(String,Box<DisplayNode>)>)
}



// Now we need to make a node builder
pub struct NodeBuilder {
    node: DisplayNode
}


impl NodeBuilder {
    pub fn new<T: ToString>(name: T, color: Color) -> Self {
        NodeBuilder {
            node: DisplayNode{
                name: Some(
                    DisplayNodeName {
                        name: name.to_string(),
                        color
                    }
                ),
                data: Terminal
            }
        }
    }

    pub fn new_unnamed() -> Self {
        NodeBuilder {
            node: DisplayNode::default()
        }
    }

    pub fn add_inline(&mut self, value: Box<DisplayNode>) -> &Self {
        self.node.data = Inline(value);
        self
    }

    pub fn make_inline<T: ToString>(&mut self,value: T, color: Color) -> &Self {
        let sub_value = Self::new(value, color).build();
        self.add_inline(sub_value)
    }

    pub fn convert_inline<T: DisplayableTree>(&mut self, value: T) -> &Self {
        self.add_inline(value.to_node())
    }

    pub fn add_child(&mut self, value: Box<DisplayNode>) -> &Self {
        match &mut self.node.data {
            _ => {
                self.node.data = List(vec![value]);
                self
            },
            List(vec) => {
                vec.push(value);
                self
            },
        }
    }

    pub fn make_child<T: ToString>(&mut self, value: T, color: Color) -> &Self {
        let sub_value = Self::new(value, color).build();
        self.add_child(sub_value)
    }

    pub fn convert_child<T: DisplayableTree>(&mut self, value: T) -> &Self {
        self.add_child(value.to_node())
    }

    pub fn add_children<T>(&mut self, value: T) -> &Self
    where
        T: Iterator<Item: DisplayableTree>
    {
        for node in value {
            self.convert_child(node)
        }
        self
    }

    pub fn add_field<T: ToString>(&mut self, field_name: T, value: Box<DisplayNode>) -> &Self {
        match &mut self.node.data {
            _ => {
                let mut map = BTreeMap::new();
                map.push((field_name.to_string(),value));
                self.node.data = Dictionary(map);
                Ok(())
            },
            Dictionary(map) => {
                map.push((field_name.to_string(),value));
                Ok(())
            }
        }
    }

    pub fn make_field<T1: ToString, T2: ToString>(&mut self, field_name: T1, value: T2, color: Color) -> &Self {
        let sub_value = Self::new(value, color).build();
        self.add_field(field_name, sub_value)
    }

    pub fn convert_field<T1: ToString, T2: DisplayableTree>(&mut self, field_name: T1, value: T2) -> &Self {
        self.add_field(field_name,value.to_node())
    }

    pub fn list_field<T1: ToString, T2>(&mut self, field_name: T1, value: T2) -> &Self
        where
            T2: Iterator<Item: DisplayableTree>
    {
        self.add_field(field_name,Self::new_unnamed_list(value))
    }

    pub fn new_terminal<T: ToString>(name: T, color: Color) -> Box<DisplayNode> {
        Self::new(name,color).build()
    }

    pub fn new_unnamed_list<T>(list: T) -> Box<DisplayNode>
        where
            T: Iterator<Item: DisplayableTree>
    {
        Self::new_unnamed().add_children(list).build()
    }
    pub fn new_binary<T: ToString, C1: DisplayableTree, C2: DisplayableTree>(name: T, color: Color, a: C1, b: C2) -> Box<DisplayNode> {
        Self::new(name,color).convert_child(a).convert_child(b).build()
    }

    pub fn build(&self) -> Box<DisplayNode> {
        Box::new(self.node.clone())
    }
}

impl Default for DisplayNode {
    fn default() -> Self {
        DisplayNode {
            name: None,
            data: Terminal
        }
    }
}

pub trait DisplayableTree {
    fn to_node(&self) -> Box<DisplayNode>;
}

impl DisplayableTree for DisplayNode {
    fn to_node(&self) -> Box<DisplayNode> {
        Box::new(self.clone())
    }
}



// Now time to implement the display functions for display nodes, given the simplified set this should all be a lot simpler for us to do, and then make modifications to as well
type PipesList = Vec<(usize,bool)>;

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

fn write_split_continued(f: &mut Formatter<'_>) -> fmt::Result {
    write!(f,"├───╮")
}

fn write_end_continued(f: &mut Formatter<'_>) -> fmt::Result {
    write!(f,"╰───╮")
}
fn write_begin(f: &mut Formatter<'_>) -> std::fmt::Result {
    // writeln!(f, "╿")
    write_empty(f)
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
impl Display for DisplayNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl DisplayNode {
    fn display_depth(&self, f: &mut Formatter<'_>, pipes: &PipesList) -> std::fmt::Result {
        // Write the name of the node
        let name_length = match &self.name {
            None => {
                if let Inline(_) = self.data {

                } else {
                    writeln!(f)?
                }
                0
            },
            Some(dnn) => {
                if let Inline(_) = self.data {
                    write!(f,"{}: ",dnn.fg(dnn.color))?
                } else
                {
                    writeln!(f, "{}", dnn.fg(dnn.color))?
                }
                dnn.name.chars().count()
            }
        };
        match &self.data {
            Terminal => Ok(()),
            Inline(node) => {
                let mut clone = pipes.clone();
                clone.last_mut().unwrap().0 += name_length+2;
                node.display_depth(f,&clone)
            }
            List(nodes) => if nodes.len() > 0 {
                let full = with_pipe(&pipes);
                let empty = with_empty(&pipes);
                for (i, node) in nodes.iter().enumerate() {
                    write_pipes(f,pipes)?;
                    (if i == 0 {write_begin} else {write_empty})(f)?;
                    write_pipes(f,pipes)?;
                    (if i == nodes.len()-1 {if let None = node.name {write_end_continued} else {write_end}} else {if let None = node.name {write_split_continued} else {write_split}})(f)?;
                    node.display_depth(f, if i == nodes.len()-1 { &empty } else { &full })?;
                }
                Ok(())
            } else {
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f,pipes)?;
                write_split(f)?;
                writeln!(f, "{}", "<empty>".fg(Color::BrightBlack))
            },
            Dictionary(nodes) => if nodes.len() > 0 {
                for (i, (name, node)) in nodes.iter().enumerate() {
                    write_pipes(f,pipes)?;
                    (if i == 0 {write_begin} else {write_empty})(f)?;
                    write_pipes(f,pipes)?;
                    (if i == nodes.len()-1 {write_end} else {write_split})(f)?;
                    write!(f, "{name}{}", if let None = node.name { "" } else {": "})?;
                    let p2 = (if i == nodes.len()-1 {with_empty_count} else {with_pipe_count})(pipes,if let None = node.name { 0} else {name.chars().count()+2 });
                    node.display_depth(f, &p2)?;
                }
                Ok(())
            } else {
                write_pipes(f, pipes)?;
                write_begin(f)?;
                write_pipes(f,pipes)?;
                write_split(f)?;
                writeln!(f, "{}", "<empty>".fg(Color::BrightBlack))
            }
        }
    }
}