// Original file: "Node.hs"
// File auto-generated using Corollary.

use std::fmt;
use std::rc::Rc;
use either::Either;

use data::name::Name;
use data::position::{Pos, Position, PosLength};

// a class for convenient access to the attributes of an attributed object
pub trait CNode {
    fn node_info(&self) -> &NodeInfo;
    fn into_node_info(self) -> NodeInfo;
}

impl CNode for NodeInfo {
    fn node_info(&self) -> &NodeInfo {
        self
    }
    fn into_node_info(self) -> NodeInfo {
        self
    }
}

impl<T: CNode, U: CNode> CNode for Either<T, U> {
    fn node_info(&self) -> &NodeInfo {
        match *self {
            Either::Left(ref x) => x.node_info(),
            Either::Right(ref x) => x.node_info()
        }
    }
    fn into_node_info(self) -> NodeInfo {
        match self {
            Either::Left(x) => x.into_node_info(),
            Either::Right(x) => x.into_node_info()
        }
    }
}

impl<T: CNode> Pos for T {
    fn pos(&self) -> &Position {
        NodeInfo::pos(self.node_info())
    }
    fn into_pos(self) -> Position {
        NodeInfo::into_pos(self.into_node_info())
    }
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
        match *self {
            NodeInfo(ref firstPos, (ref lastPos, lastTokLen), _) |
            OnlyPos( ref firstPos, (ref lastPos, lastTokLen)) => if lastTokLen < 0 {
                None
            } else {
                Some(lastPos.offset() + lastTokLen - firstPos.offset())
            }
        }
    }

    pub fn get_last_token_pos(&self) -> &PosLength {
        match *self {
            NodeInfo(_, ref last, _) => last,
            OnlyPos(_, ref last) => last,
        }
    }

    pub fn name(&self) -> Option<Name> {
        match *self {
            OnlyPos(_, _) => None,
            NodeInfo(_, _, name) => Some(name),
        }
    }

    // NOTE: these are not an impl of Pos because that impl is automatic
    // for all CNodes and falls back to these inherent methods!

    fn pos(&self) -> &Position {
        match *self {
            OnlyPos(ref pos, _) => pos,
            NodeInfo(ref pos, _, _) => pos,
        }
    }

    fn into_pos(self) -> Position {
        match self {
            OnlyPos(pos, _) => pos,
            NodeInfo(pos, _, _) => pos,
        }
    }
}

pub fn fileOfNode<A: CNode>(obj: &A) -> Option<Rc<String>> {
    let pos = obj.pos();
    if pos.isSource() {
        Some(pos.file())
    } else {
        None
    }
}

pub fn eqByName<A: CNode>(obj1: A, obj2: A) -> bool {
    obj1.node_info() == obj2.node_info()
}
