// Original file: "Position.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Data::Generics;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum Position {
    Position {
        offset: isize,
        file: String,
        row: isize,
        column: isize,
    },
    NoPosition,
    BuiltinPosition,
    InternalPosition,
}
pub use self::Position::{NoPosition, BuiltinPosition, InternalPosition};

impl ::std::fmt::Display for Position {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Position {
    pub fn new(offset: isize, file: String, row: isize, column: isize) -> Position {
        Position::Position { offset, file, row, column }
    }

    pub fn from_file(file: FilePath) -> Position {
        Self::new(0, file.into(), 1, 1)
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

    pub fn file(&self) -> String {
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
                Self::new(offset, new_file.into(), row, 1),
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

// class of type which aggregate a source code location
pub trait Pos {
    fn posOf(self) -> Position;
}
pub fn posOf<P: Pos>(input: P) -> Position {
    input.posOf()
}
