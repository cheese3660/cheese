use std::rc::Rc;
use std::sync::Mutex;
use ariadne::Source;
use lazy_static::lazy_static;

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

impl Default for File {
    fn default() -> Self {
        let owned = "".to_string();
        File {
            filename: "<default file>".to_string(),
            data: "".to_string(),
            source: Source::from(owned)
        }
    }
}

thread_local! {
    pub static DEFAULT_FILE: Rc<File> = Rc::new(File::default());
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

impl Default for Coordinate {
    fn default() -> Self {
        Coordinate {
            file: DEFAULT_FILE.with(|file| file.clone()),
            pos: 0
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

impl Default for FileSpan {
    fn default() -> Self {
        Self::new(Coordinate::default(), Coordinate::default())
    }
}