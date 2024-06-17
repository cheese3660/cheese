use crate::strings::UnescapeState::{Hex2, Hex4, Hex8, Normal, WaitingForEscapeChar};

pub trait Escapable {
    fn escape(&self) -> Self;
    fn unescape(&self) -> Self;
    fn escape_with_quotes(&self, quote: &str) -> Self;

    fn unescape_with_quotes(&self) -> Self;
}

enum UnescapeState {
    Normal,
    WaitingForEscapeChar,
    Hex2(u8, isize),
    Hex4(u16, isize),
    Hex8(u32, isize),
}

impl Escapable for String {
    fn escape(&self) -> Self {
        let mut result = "".to_string();
        for ch in self.chars() {
           match ch {
                ' '..='~' =>
                    if ch == '\\' {
                        result += "\\\\"
                    } else if ch == '"' {
                        result += "\\\""
                    } else if ch == '\'' {
                        result += "\\'"
                    } else {
                        result.push(ch);
                    },
                '\n' => result += "\\n",
                '\r' => result += "\\r",
                '\t' => result += "\\t",
                _ => {
                    let codepoint = ch as u32;
                    result += if codepoint < 256 {
                        format!("\\x{:02x}", codepoint)
                    } else if codepoint < 65536 {
                        format!("\\u{:04x}", codepoint)
                    } else {
                        format!("\\U{:08x}", codepoint)
                    }.as_str()
                }
            }
        }
        result
    }

    fn unescape(&self) -> Self {
        let mut result = "".to_string();
        let mut state = Normal;
        for ch in self.chars() {
            state = match state {
                Normal => if ch == '\\' {
                    WaitingForEscapeChar
                } else {
                    result.push(ch);
                    Normal
                },
                WaitingForEscapeChar => match ch {
                    'a' => {
                        result += "\x07";
                        Normal
                    }
                    'b' => {
                        result += "\x08";
                        Normal
                    }
                    'e' => {
                        result += "\x1B";
                        Normal
                    }
                    'n' => {
                        result += "\n";
                        Normal
                    }
                    'r' => {
                        result += "\r";
                        Normal
                    }
                    't' => {
                        result += "\t";
                        Normal
                    }
                    'v' => {
                        result += "\x0B";
                        Normal
                    }
                    '\\' => {
                        result += "\\";
                        Normal
                    }
                    '\'' => {
                        result += "\'";
                        Normal
                    }
                    '\"' => {
                        result += "\"";
                        Normal
                    }
                    '?' => {
                        result += "?";
                        Normal
                    }
                    'x' => Hex2(0, 0),
                    'u' => Hex4(0, 0),
                    'U' => Hex8(0, 0),
                    _ => {
                        result.push(ch);
                        Normal
                    }
                },
                Hex2(mut current, mut count) =>
                    if ch.is_ascii_hexdigit() {
                        current <<= 4;
                        current += ch.to_digit(16).unwrap() as u8;
                        count += 1;
                        if count == 2 {
                            result.push(current as char);
                            Normal
                        } else {
                            Hex2(current, count)
                        }
                    } else {
                        result.push(current as char);
                        result += ch.to_string().as_str();
                        Normal
                    },
                Hex4(mut current, mut count) =>
                    if ch.is_ascii_hexdigit() {
                        current <<= 4;
                        current += ch.to_digit(16).unwrap() as u16;
                        count += 1;
                        if count == 4 {
                            result.push(std::char::from_u32(current as u32).unwrap());
                            Normal
                        } else {
                            Hex4(current, count)
                        }
                    } else {
                        result.push(std::char::from_u32(current as u32).unwrap());
                        result += ch.to_string().as_str();
                        Normal
                    }
                Hex8(mut current, mut count) =>
                    if ch.is_ascii_hexdigit() {
                        current <<= 4;
                        current += ch.to_digit(16).unwrap();
                        count += 1;
                        if count == 8 {
                            result.push(std::char::from_u32(current).unwrap());
                            Normal
                        } else {
                            Hex8(current, count)
                        }
                    } else {
                        result.push(std::char::from_u32(current).unwrap());
                        result += ch.to_string().as_str();
                        Normal
                    }
            }
        }
        match state {
            Hex2(current, _) => result.push(current as char),
            Hex4(current, _) => result.push(std::char::from_u32(current as u32).unwrap()),
            Hex8(current, _) => result.push(std::char::from_u32(current).unwrap()),
            _ => {},
        }
        result
    }

    fn escape_with_quotes(&self, quote: &str) -> Self {
        quote.to_string() + self.escape().as_str() + quote
    }

    fn unescape_with_quotes(&self) -> Self {
        let char_len = self.chars().count();
        let result: String = self.chars().skip(1).take(char_len - 2).collect();
        result.unescape()
    }
}