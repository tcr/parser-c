// Original file: "Position.hs"
// File auto-generated using Corollary.

use std::fmt;
use std::rc::Rc;
use std::path::Path;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum Position {
    Position {
        offset: usize,
        file: Rc<String>,
        row: usize,
        column: usize,
    },
    NoPosition,
    BuiltinPosition,
    InternalPosition,
}
pub use self::Position::{NoPosition, BuiltinPosition, InternalPosition};

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NoPosition => write!(f, "<no file>"),
            BuiltinPosition => write!(f, "<builtin>"),
            InternalPosition => write!(f, "<internal>"),
            Position::Position { offset, ref file, row, column } => {
                write!(f, "{} line {}, column {} (source-offset {})",
                       file, row, column, offset)
            }
        }
    }
}

impl Position {
    pub fn new(offset: usize, file: Rc<String>, row: usize, column: usize) -> Position {
        Position::Position { offset, file: file, row, column }
    }

    pub fn from_file<P: AsRef<Path>>(file: P) -> Position {
        let path_str = file.as_ref().display().to_string();
        Position::Position { offset: 1, file: Rc::new(path_str), row: 1, column: 1 }
    }

    pub fn none() -> Position {
        NoPosition
    }

    pub fn builtin() -> Position {
        BuiltinPosition
    }

    pub fn internal() -> Position {
        InternalPosition
    }

    pub fn offset(&self) -> usize {
        if let Position::Position { offset, .. } = *self {
            offset
        } else {
            panic!("Non Position::Position passed to posOffset")
        }
    }

    pub fn file(&self) -> Rc<String> {
        if let Position::Position { ref file, .. } = *self {
            file.clone()
        } else {
            panic!("Non Position::Position passed to posFile")
        }
    }

    pub fn row(&self) -> usize {
        if let Position::Position { row, .. } = *self {
            row
        } else {
            panic!("Non Position::Position passed to posRow")
        }
    }

    pub fn column(&self) -> usize {
        if let Position::Position { column, .. } = *self {
            column
        } else {
            panic!("Non Position::Position passed to posColumn")
        }
    }

    pub fn isSource(&self) -> bool {
        match *self {
            Position::Position { .. } => true,
            _ => false,
        }
    }

    pub fn isNone(&self) -> bool {
        self == &NoPosition
    }

    pub fn isBuiltin(&self) -> bool {
        self == &BuiltinPosition
    }

    pub fn isInternal(&self) -> bool {
        self == &InternalPosition
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

    pub fn adjust(self, new_file: String, row: usize) -> Position {
        match self {
            Position::Position { offset, .. } =>
                Self::new(offset, Rc::new(new_file), row, 1),
            p => p,
        }
    }
}

pub type PosLength = (Position, usize);

// class of types which aggregate a source code location
pub trait Pos {
    fn pos(&self) -> &Position;
    fn into_pos(self) -> Position;
}

// convenient instance, the position of a list of things is the position of
// the first thing in the list

impl<A: Pos> Pos for Vec<A> {
    fn pos(&self) -> &Position {
        self.get(0).map_or(&Position::NoPosition, |v| v.pos())
    }
    fn into_pos(mut self) -> Position {
        if !self.is_empty() {
            self.swap_remove(0).into_pos()
        } else {
            NoPosition
        }
    }
}

// We occasionally need things to have a location when they don't naturally
// have one built in as tokens and most AST elements do.

#[derive(Clone)]
pub struct Located<T>(T, Position);

impl<T> Pos for Located<T> {
    fn pos(&self) -> &Position {
        &self.1
    }
    fn into_pos(self) -> Position {
        self.1
    }
}

impl<T> Located<T> {
    pub fn new<P: Pos>(t: T, pos: P) -> Located<T> {
        Located(t, pos.into_pos())
    }
    pub fn into_inner(self) -> T {
        self.0
    }
}
