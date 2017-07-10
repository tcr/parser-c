// Original file: "Node.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Language::C::Data::Position;
// use Language::C::Data::Name;
// use Name;
// use Data::Generics;

use data::name::Name;
use data::position::*;
use std::fmt;


// a class for convenient access to the attributes of an attributed object
pub trait CNode {
    fn nodeInfo(self) -> NodeInfo;
}

impl CNode for NodeInfo {
    fn nodeInfo(self) -> NodeInfo { self }
}

pub fn nodeInfo<T: CNode>(n: T) -> NodeInfo {
    n.nodeInfo()
}

#[derive(Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum NodeInfo {
    OnlyPos(Position, PosLength),
    NodeInfo(Position, PosLength, Name),
}
pub use self::NodeInfo::*;

// TODO This should be replaced with a better impl
impl fmt::Debug for NodeInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "..")
    }
}

impl Pos for NodeInfo {
    fn posOf(self) -> Position {
        match self {
            NodeInfo::OnlyPos(pos, _) => pos,
            NodeInfo::NodeInfo(pos, _, _) => pos
        }
    }
}

impl NodeInfo {
    pub fn internal() -> NodeInfo {
        Self::undef()
    }

    pub fn undef() -> NodeInfo {
        OnlyPos(Position::none(), (Position::none(), -1))
    }

    pub fn new(pos: Position, lasttok: PosLength, name: Name) -> NodeInfo {
        NodeInfo(pos, lasttok, name)
    }

    pub fn with_only_pos(pos: Position) -> NodeInfo {
        OnlyPos(pos, (Position::none(), -1))
    }

    pub fn with_pos_len(a: Position, b: PosLength) -> NodeInfo {
        OnlyPos(a, b)
    }

    pub fn with_pos_name(pos: Position, name: Name) -> NodeInfo {
        NodeInfo(pos, (Position::none(), -1), name)
    }

    pub fn len(&self) -> Option<isize> {
        let computeLength = |pos: &Position, &(ref lastPos, lastTokLen): &PosLength| {
            if lastTokLen < 0 {
                None
            } else {
                Some(lastPos.offset() + lastTokLen - pos.offset())
            }
        };

        let len = match *self {
            NodeInfo(ref firstPos, ref lastTok, _) |
            OnlyPos(ref firstPos, ref lastTok) => computeLength(firstPos, lastTok),
        };

        len
    }

    pub fn getLastTokenPos(self) -> PosLength {
        match self {
            NodeInfo(_, lastTok, _) => lastTok,
            OnlyPos(_, lastTok) => lastTok,
        }
    }

    pub fn name(&self) -> Option<Name> {
        match *self {
            OnlyPos(_, _) => None,
            NodeInfo(_, _, name) => Some(name),
        }
    }

    // TODO: necessary, or is pos_ref enough?
    pub fn pos(self) -> Position {
        match self {
            OnlyPos(pos, _) => pos,
            NodeInfo(pos, _, _) => pos,
        }
    }

    pub fn pos_ref(&self) -> &Position {
        match *self {
            OnlyPos(ref pos, _) => pos,
            NodeInfo(ref pos, _, _) => pos,
        }
    }
}

pub fn fileOfNode<A: CNode>(obj: A) -> Option<FilePath> {
    let pos = obj.nodeInfo().pos();
    if pos.isSource() {
        Some(FilePath { path: pos.file() })
    } else {
        None
    }
}

pub fn eqByName<A: CNode>(obj1: A, obj2: A) -> bool {
    obj1.nodeInfo() == obj2.nodeInfo()
}
