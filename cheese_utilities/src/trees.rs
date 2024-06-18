// This is an entire tree drawing module, using a Node

use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::Bound;
use ariadne::{Color, Fmt};
use crate::trees::DisplayNodeData::{Dictionary, Inline, List, Terminal};


#[derive(Debug, Clone)]
pub struct DisplayNode {
    name: Option<DisplayNodeName>,
    data: DisplayNodeData,
}

#[derive(Debug, Clone)]
pub struct DisplayNodeName {
    name: String,
    color: Color,
}

#[derive(Debug, Clone)]
pub enum DisplayNodeData {
    Terminal,
    Inline(Box<DisplayNode>),
    List(Vec<Box<DisplayNode>>),
    Dictionary(Vec<(String, Box<DisplayNode>)>),
}


// Now we need to make a node builder
pub struct NodeBuilder {
    node: DisplayNode,
}


impl NodeBuilder {
    pub fn new<T: ToString>(name: T, color: Color) -> Self {
        NodeBuilder {
            node: DisplayNode {
                name: Some(
                    DisplayNodeName {
                        name: name.to_string(),
                        color,
                    }
                ),
                data: Terminal,
            }
        }
    }

    pub fn new_unnamed() -> Self {
        NodeBuilder {
            node: DisplayNode::default()
        }
    }

    pub fn add_inline(&mut self, value: Box<DisplayNode>) -> &mut Self {
        self.node.data = Inline(value);
        self
    }

    pub fn make_inline<T: ToString>(&mut self, value: T, color: Color) -> &mut Self {
        let sub_value = Self::new(value, color).build();
        self.add_inline(sub_value)
    }

    pub fn convert_inline<T: DisplayableTree>(&mut self, value: T) -> &mut Self {
        self.add_inline(value.to_node())
    }

    pub fn add_child(&mut self, value: Box<DisplayNode>) -> &mut Self {
        match &mut self.node.data {
            List(vec) => {
                vec.push(value);
                self
            }
            _ => {
                self.node.data = List(vec![value]);
                self
            }
        }
    }

    pub fn make_child<T: ToString>(&mut self, value: T, color: Color) -> &mut Self {
        let sub_value = Self::new(value, color).build();
        self.add_child(sub_value)
    }

    pub fn convert_child<T: DisplayableTree>(&mut self, value: T) -> &mut Self {
        self.add_child(value.to_node())
    }

    pub fn add_children<T: Iterator>(&mut self, value: T) -> &mut Self
        where
            <T as Iterator>::Item: DisplayableTree
    {
        self.node.data = List(vec![]);
        for node in value {
            self.convert_child(node);
        }
        self
    }

    pub fn add_field<T: ToString>(&mut self, field_name: T, value: Box<DisplayNode>) -> &mut Self {
        match &mut self.node.data {
            Dictionary(map) => {
                map.push((field_name.to_string(), value));
            }
            _ => {
                let mut map = vec![];
                map.push((field_name.to_string(), value));
                self.node.data = Dictionary(map);
            }
        }
        self
    }

    pub fn make_field<T1: ToString, T2: ToString>(&mut self, field_name: T1, value: T2, color: Color) -> &mut Self {
        let sub_value = Self::new(value, color).build();
        self.add_field(field_name, sub_value)
    }

    pub fn convert_field<T1: ToString, T2: DisplayableTree>(&mut self, field_name: T1, value: T2) -> &mut Self {
        self.add_field(field_name, value.to_node())
    }

    pub fn list_field<T1: ToString, T2: Iterator>(&mut self, field_name: T1, value: T2) -> &mut Self
        where
            <T2 as Iterator>::Item: DisplayableTree
    {
        self.add_field(field_name, Self::new_unnamed_list(value))
    }

    pub fn new_terminal<T: ToString>(name: T, color: Color) -> Box<DisplayNode> {
        Self::new(name, color).build()
    }

    pub fn new_unnamed_list<T: Iterator>(list: T) -> Box<DisplayNode>
        where
            <T as Iterator>::Item: DisplayableTree
    {
        Self::new_unnamed().add_children(list).build()
    }
    pub fn new_binary<T: ToString, C1: DisplayableTree, C2: DisplayableTree>(name: T, color: Color, a: C1, b: C2) -> Box<DisplayNode> {
        let mut builder = Self::new(name, color);
        builder.convert_child(a);
        builder.convert_child(b);
        builder.build()
    }

    pub fn ensure_dictionary(&mut self) -> &mut Self {
        self.node.data = Dictionary(vec![]);
        self
    }

    pub fn build(&self) -> Box<DisplayNode> {
        Box::new(self.node.clone())
    }
}

impl Default for DisplayNode {
    fn default() -> Self {
        DisplayNode {
            name: None,
            data: Terminal,
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

impl<T> DisplayableTree for &T
    where T: DisplayableTree
{
    fn to_node(&self) -> Box<DisplayNode> {
        T::to_node(self)
    }
}

impl<T> DisplayableTree for Box<T>
    where T: DisplayableTree
{
    fn to_node(&self) -> Box<DisplayNode> {
        T::to_node(self)
    }
}

impl Display for DisplayNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for line in self.generate_bounded_lines(None).0 {
            writeln!(f, "{line}")?;
        }
        Ok(())
//        let vec = vec![];
//        self.display_depth(f,&vec)
    }
}

struct BoundedLine {
    line: String,
    begin: usize,
    end: usize,
    color_spans: Vec<(usize, usize, Color)>,
}

impl Display for BoundedLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut span_iter = self.color_spans.iter();
        let mut current_span = span_iter.next();
        for (i, c) in self.line.chars().enumerate() {
            match current_span {
                None => write!(f, "{}", c)?,
                Some((begin, end, color)) => if i >= *begin && i < *end {
                    write!(f, "{}", c.fg(*color))?;
                } else if i >= *end {
                    write!(f, "{}", c)?;
                    current_span = span_iter.next()
                } else {
                    write!(f, "{}", c)?
                }
            }
        }
        Ok(())
    }
}

fn count_whitespace_chars_at_start(input: &str) -> usize {
    input
        .chars()
        .take_while(|ch| ch.is_whitespace())
        .count()
}

impl BoundedLine {
    fn new<T: ToString>(value: T) -> Self {
        let binding = value.to_string();
        let line = binding.trim_end();
        let begin = count_whitespace_chars_at_start(line);
        let end = line.chars().count();
        BoundedLine {
            line: line.to_string(),
            begin,
            end,
            color_spans: vec![],
        }
    }

    fn set_color(&mut self, c: Color) {
        self.color_spans.push((self.begin, self.end, c))
    }

    fn with_prefix<T: ToString>(&self, prefix: T) -> Self {
        let s = prefix.to_string();
        let chars = s.chars().count();
        let mut result = Self::new(s + self.line.as_str());
        for (begin, end, color) in &self.color_spans {
            result.color_spans.push((begin + chars, end + chars, *color));
        }
        result
    }

    fn collides_with(&self, other: &BoundedLine) -> bool {
        self.end + 1 >= other.begin
    }

    fn combine(&mut self, other: BoundedLine) {
        self.begin = other.begin;
        self.line = other.line + self.line.chars().skip(other.end).collect::<String>().as_str();
        for span in other.color_spans.iter().rev() {
            self.color_spans.insert(0, *span)
        }
    }
}

fn shift_lines(lines: &mut Vec<BoundedLine>, prefix: String) {
    if lines.len() == 0 {
        lines.push(BoundedLine::new(prefix))
    } else {
        let spaces: String = prefix.chars().map(|_| ' ').collect();
        let mut i = 0;
        while i < lines.len() {
            if i == 0 {
                lines[i] = lines[i].with_prefix(prefix.as_str());
            } else {
                lines[i] = lines[i].with_prefix(spaces.as_str());
            }
            i += 1;
        }
    }
}

// Returns true if they collide
fn check_collision(a: &Vec<BoundedLine>, b: &Vec<BoundedLine>, offset: usize) -> bool {
    let mut i = 0;
    while i < b.len() {
        let j = i + offset;
        if j >= a.len() {
            break;
        }
        if b[i].collides_with(&a[j]) {
            return true;
        }

        i += 1;
    }
    false
}


fn combine_at(lines: &mut Vec<BoundedLine>, combination: Vec<BoundedLine>, offset: usize) {
    let mut i = offset;
    for line in combination {
        if i < lines.len() {
            lines[i].combine(line)
        } else {
            while i > lines.len() {
                lines.push(BoundedLine::new(""));
            }
            lines.push(line);
        }
        i += 1;
    }
}

// This returns the line number that the new set starts at now
fn combine_lines(lines: &mut Vec<BoundedLine>, mut combination: Vec<BoundedLine>, last_start: usize) -> usize {
    if lines.len() == 0 {
        lines.append(&mut combination);
        return 0;
    }

    let mut start_loc = lines.len() + 2;
    let end_loc = loop {
        if start_loc <= last_start + 2 {
            break last_start + 2;
        }
        let collision_check = check_collision(lines, &combination, start_loc - 2);
        if collision_check {
            break start_loc;
        }
        start_loc -= 1;
    };
    //let end_loc = start_loc;
    combine_at(lines, combination, end_loc);
    end_loc
}

fn print_lines(lines: &Vec<BoundedLine>) {
    for line in lines {}
}

impl DisplayNode {
    fn generate_bounded_lines(&self, dictionary_name: Option<String>) -> (Vec<BoundedLine>, bool) {
        #[derive(Debug)]
        enum PrefixType {
            Split(bool),
            End(bool),
        }

        let mut lines = match &self.data {
            Terminal => vec![],
            Inline(value) => value.generate_bounded_lines(None).0,
            List(values) => {
                let mut lines = vec![];
                let mut prefix_locations = HashMap::new();
                let mut last_start = 0;
                for (i, value) in values.iter().enumerate() {
                    let (new_lines, nameless) = value.generate_bounded_lines(None);
                    last_start = combine_lines(&mut lines, new_lines, 0);
                    prefix_locations.insert(last_start, if i == values.len() - 1 {
                        PrefixType::End(nameless)
                    } else {
                        PrefixType::Split(nameless)
                    });
                }
                if values.len() == 0 {
                    let mut empty_marker = BoundedLine::new("<empty>");
                    empty_marker.set_color(Color::BrightBlack);
                    lines = vec![empty_marker];
                    prefix_locations.insert(0, PrefixType::End(false));
                }
                let mut location = 0;
                let mut past_end = false;
                while location < lines.len() {
                    lines[location] = match prefix_locations.get(&location) {
                        None => if past_end {
                            lines[location].with_prefix("    ")
                        } else {
                            lines[location].with_prefix("│   ")
                        }
                        Some(prefix) => lines[location].with_prefix(match prefix {
                            PrefixType::Split(true) => "├───",
                            PrefixType::Split(false) => "├─► ",
                            PrefixType::End(true) => {
                                past_end = true;
                                "╰───"
                            }
                            PrefixType::End(false) => {
                                past_end = true;
                                "╰─► "
                            }
                        })
                    };
                    location += 1;
                }
                lines.insert(0, BoundedLine::new("│"));
                lines
            }
            Dictionary(values) => {
                let mut lines = vec![];
                let mut prefix_locations = HashMap::new();
                let mut last_start = 0;
                for (i, (name, value)) in values.iter().enumerate() {
                    let (mut new_lines, nameless) = value.generate_bounded_lines(Some(name.clone()));
                    if value.name.is_some() {
                        shift_lines(&mut new_lines, name.to_string() + ": ");
                    }
                    // for line in &new_lines {
                    //     
                    // }


                    last_start = combine_lines(&mut lines, new_lines, last_start);
                    prefix_locations.insert(last_start, if i == values.len() - 1 {
                        PrefixType::End(nameless)
                    } else {
                        PrefixType::Split(nameless)
                    });
                }

                if values.len() == 0 {
                    let mut empty_marker = BoundedLine::new("<empty>");
                    empty_marker.set_color(Color::BrightBlack);
                    lines = vec![empty_marker];
                    prefix_locations.insert(0, PrefixType::End(false));
                }
                let mut location = 0;
                let mut past_end = false;
                while location < lines.len() {
                    lines[location] = match prefix_locations.get(&location) {
                        None => if past_end {
                            lines[location].with_prefix("    ")
                        } else {
                            lines[location].with_prefix("│   ")
                        },
                        Some(ty) => lines[location].with_prefix(match ty {
                            PrefixType::Split(_) => "├─► ",
                            PrefixType::End(_) => {
                                past_end = true;
                                "╰─► "
                            }
                        })
                    };
                    location += 1;
                }
                lines.insert(0, BoundedLine::new("│"));
                lines
            }
        };
        if let Inline(_) = self.data {
            let n = self.name.clone().unwrap();
            let name = n.name;
            let length = name.chars().count();
            shift_lines(&mut lines, name + ": ");
            lines[0].color_spans.insert(0, (0, length, n.color));
        } else {
            let name_bound = match &self.name {
                None => match dictionary_name {
                    None => BoundedLine::new("╮"),
                    Some(dict_name) => BoundedLine::new(dict_name),
                }
                Some(name) => {
                    let mut line = BoundedLine::new(&name.name);
                    line.set_color(name.color);
                    line
                }
            };
            lines.insert(0, name_bound);
        }
        (lines, self.name.is_none())
    }
}