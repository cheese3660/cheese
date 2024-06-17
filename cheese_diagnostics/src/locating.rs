use std::rc::Rc;
use ariadne::Source;

#[derive(Clone, Debug)]
pub struct File {
    pub filename: String,
    pub data: String,
    pub source: Source<String>
}

impl File {
    pub fn new(filename: &str, data: &str) -> Rc<File> {
        let owned = data.to_string();
        Rc::new(File {
            filename: filename.to_string(),
            data: owned.clone(),
            source: Source::from(owned)
        })
    }
}

#[derive(Debug)]
pub struct Coordinate {
    pub file: Rc<File>,
    pub pos: usize,
}

impl Clone for Coordinate {
    fn clone(&self) -> Self {
        Coordinate {
            file: Rc::clone(&self.file),
            pos: self.pos
        }
    }
}


#[derive(Clone, Debug)]
pub struct FileSpan {
    pub begin: Coordinate,
    pub end: Coordinate,
}

impl FileSpan {
    pub fn new(begin: Coordinate, end: Coordinate) -> Self {
        FileSpan{
            begin,
            end
        }
    }

    pub fn expanded(&self, end_span: &FileSpan) -> Self {
        FileSpan {
            begin: self.begin.clone(),
            end: end_span.end.clone()
        }
    }
}