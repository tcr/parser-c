// Original file: "Position.hs"
// File auto-generated using Corollary.

use corollary_support::FilePath;

use std::sync::Arc;

// TODO use a deque
use data::r_list::Reversed;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum Position {
    Position {
        offset: isize,
        file: Arc<String>,
        row: isize,
        column: isize,
    },
    NoPosition,
    BuiltinPosition,
    InternalPosition,
}
pub use self::Position::{NoPosition, BuiltinPosition, InternalPosition};

// XXX: replace this later
impl ::std::fmt::Display for Position {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Position {
    pub fn new(offset: isize, file: Arc<String>, row: isize, column: isize) -> Position {
        Position::Position { offset, file: file, row, column }
    }

    pub fn from_file(file: FilePath) -> Position {
        Position::Position { offset: 0, file: Arc::new(file.into()), row: 1, column: 1 }
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

    pub fn offset(&self) -> isize {
        if let Position::Position { offset, .. } = *self {
            offset
        } else {
            panic!("Non Position::Position passed to posOffset")
        }
    }

    pub fn file(&self) -> Arc<String> {
        if let Position::Position { ref file, .. } = *self {
            file.clone()
        } else {
            panic!("Non Position::Position passed to posFile")
        }
    }

    pub fn row(&self) -> isize {
        if let Position::Position { row, .. } = *self {
            row
        } else {
            panic!("Non Position::Position passed to posRow")
        }
    }

    pub fn column(&self) -> isize {
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

    pub fn inc(self, by: isize) -> Position {
        match self {
            Position::Position { offset, file, row, column } =>
                Self::new(offset + by, file, row, column + by),
            p => p,
        }
    }

    pub fn retPos(self) -> Position {
        match self {
            Position::Position { offset, file, row, .. } =>
                Self::new(offset + 1, file, row + 1, 1),
            p => p,
        }
    }

    pub fn adjust(self, new_file: FilePath, row: isize) -> Position {
        match self {
            Position::Position { offset, .. } =>
                Self::new(offset, Arc::new(new_file.into()), row, 1),
            p => p,
        }
    }

    pub fn incOffset(self, by: isize) -> Position {
        match self {
            Position::Position { offset, file, row, column } =>
                Self::new(offset + by, file, row, column),
            p => p,
        }
    }
}

pub type PosLength = (Position, isize);

// class of types which aggregate a source code location
pub trait Pos {
    fn pos(&self) -> &Position;
    fn into_pos(self) -> Position;
}

// convenient instance, the position of a list of things is the position of
// the first thing in the list

impl<A: Pos> Pos for Vec<A> {
    fn pos(&self) -> &Position {
        self[0].pos()
    }
    fn into_pos(mut self) -> Position {
        self.remove(0).into_pos()
    }
}

impl<A: Pos> Pos for Reversed<A> {
    fn pos(&self) -> &Position {
        (self.0).pos()
    }
    fn into_pos(self) -> Position {
        (self.0).into_pos()
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
