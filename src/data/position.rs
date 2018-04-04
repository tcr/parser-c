// Original file: "Position.hs"
// File auto-generated using Corollary.

use std::fmt;
use std::rc::Rc;
use std::path::Path;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum Position {
    Position {
        /// Absolute offset in the preprocessed file.
        offset: usize,
        /// Original file name. Affected by #line pragmas.
        file: Rc<str>,
        /// Line in the original file. Affected by #line pragmas.
        row: usize,
        /// Column in the original file. Affected by #line pragmas.
        column: usize,
        /// Including file, if any (forms #line stack).
        parent: Option<Rc<Position>>,
    },
    None,
    Builtin,
    Internal,
}

fn parent_display(pos: &Position, f: &mut fmt::Formatter) -> fmt::Result {
    match *pos {
        Position::None | Position::Builtin | Position::Internal => {
            write!(f, "In file included from {}", pos)
        }
        Position::Position { ref file, row, column, parent: None, .. } => {
            write!(f, "In file included from {}:{}:{}", file, row, column)
        }
        Position::Position { ref file, row, column, parent: Some(ref parent), .. } => {
            parent_display(parent, f)?;
            write!(f, ",\n                 from {}:{}:{}", file, row, column)
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Position::None => write!(f, "<no file>"),
            Position::Builtin => write!(f, "<builtin>"),
            Position::Internal => write!(f, "<internal>"),
            Position::Position { offset, ref file, row, column, ref parent } => {
                if let Some(ref parent) = *parent {
                    parent_display(parent, f)?;
                    write!(f, ":\n")?;
                }
                write!(f, "{}:{}:{} (offset {})", file, row, column, offset)
            }
        }
    }
}

impl Position {
    pub fn new(offset: usize, file: Rc<str>, row: usize, column: usize,
               parent: Option<Rc<Position>>) -> Position {
        Position::Position { offset, file, row, column, parent }
    }

    pub fn from_file<P: AsRef<Path>>(file: P) -> Position {
        let path_str = file.as_ref().display().to_string();
        Position::Position { offset: 1, file: path_str.into(),
                             row: 1, column: 1, parent: None }
    }

    pub fn none() -> Position {
        Position::None
    }

    pub fn builtin() -> Position {
        Position::Builtin
    }

    pub fn internal() -> Position {
        Position::Internal
    }

    pub fn offset(&self) -> Option<usize> {
        if let Position::Position { offset, .. } = *self {
            Some(offset)
        } else {
            None
        }
    }

    pub fn file(&self) -> Option<Rc<str>> {
        if let Position::Position { ref file, .. } = *self {
            Some(file.clone())
        } else {
            None
        }
    }

    pub fn row(&self) -> Option<usize> {
        if let Position::Position { row, .. } = *self {
            Some(row)
        } else {
            None
        }
    }

    pub fn column(&self) -> Option<usize> {
        if let Position::Position { column, .. } = *self {
            Some(column)
        } else {
            panic!("Non Position::Position passed to posColumn")
        }
    }

    pub fn parent(&self) -> Option<Rc<Position>> {
        if let Position::Position { ref parent, .. } = *self {
            parent.clone()
        } else {
            None
        }
    }

    pub fn is_source(&self) -> bool {
        match *self {
            Position::Position { .. } => true,
            _ => false,
        }
    }

    pub fn is_none(&self) -> bool {
        self == &Position::None
    }

    pub fn is_builtin(&self) -> bool {
        self == &Position::Builtin
    }

    pub fn is_internal(&self) -> bool {
        self == &Position::Internal
    }

    pub fn inc_chars(&mut self, by: usize) {
        if let Position::Position { ref mut offset, ref mut column, .. } = *self {
            *offset += by;
            *column += by;
        }
    }

    pub fn inc_offset(&mut self, by: usize) {
        if let Position::Position { ref mut offset, .. } = *self {
            *offset += by;
        }
    }

    pub fn inc_newline(&mut self) {
        if let Position::Position { ref mut offset, ref mut row, ref mut column, .. } = *self {
            *offset += 1;
            *row += 1;
            *column = 1;
        }
    }
}

pub type PosLength = (Rc<Position>, usize);

// class of types which aggregate a source code location
pub trait Pos {
    fn pos(&self) -> Rc<Position>;
}

// convenient instance, the position of a list of things is the position of
// the first thing in the list

impl<A: Pos> Pos for Vec<A> {
    fn pos(&self) -> Rc<Position> {
        if !self.is_empty() {
            self[0].pos()
        } else {
            Rc::new(Position::None)
        }
    }
}

// We occasionally need things to have a location when they don't naturally
// have one built in as tokens and most AST elements do.

#[derive(Clone)]
pub struct Located<T>(T, Rc<Position>);

impl<T> Pos for Located<T> {
    fn pos(&self) -> Rc<Position> {
        self.1.clone()
    }
}

impl<T> Located<T> {
    pub fn new<P: Pos>(t: T, pos: P) -> Located<T> {
        Located(t, pos.pos())
    }
    pub fn into_inner(self) -> T {
        self.0
    }
}
