// Original file: "Ident.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Data::Char;
// use Language::C::Data::Position;
// use Language::C::Data::Node;
// use Language::C::Data::Name;
// use Name;
// use Data::Generics;

use std::hash::{Hash, Hasher};

use data::position::*;
use data::node::*;
use data::name::Name;

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq)]
pub enum SUERef {
    AnonymousRef(Name),
    NamedRef(Ident),
}
pub use self::SUERef::*;

impl SUERef {
    pub fn is_anonymous(&self) -> bool {
        match *self {
            AnonymousRef(_) => true,
            _ => false,
        }
    }

    pub fn to_string(self) -> String {
        match self {
            AnonymousRef(_) => "".into(),
            NamedRef(ident) => ident.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialOrd, Eq)]
pub struct Ident(pub String, pub NodeInfo);

// required because we keep Idents in a HashSet and don't want the set to
// consider the NodeInfo part important for comparison
impl Hash for Ident {
    fn hash<H: Hasher>(&self, h: &mut H) {
        (self.0).hash(h);
    }
}

// the definition of the equality allows identifiers to be equal that are
// defined at different source text positions
impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

// -- identifiers are attributed
impl CNode for Ident {
    fn nodeInfo(self) -> NodeInfo {
        let Ident(_, at) = self;
        at
    }
}

impl Pos for Ident {
    fn posOf(self) -> Position {
        nodeInfo(self).into_pos()
    }
}

impl Ident {
    pub fn new(pos: Position, s: String, name: Name) -> Ident {
        let len = s.len() as isize;
        Ident(s, NodeInfo::new(pos.clone(), (pos, len), name))
    }

    pub fn internal(s: String) -> Ident {
        Ident(s, NodeInfo::with_only_pos(Position::internal()))
    }

    pub fn internal_at(pos: Position, s: String) -> Ident {
        let len = s.len() as isize;
        Ident(s, NodeInfo::with_pos_len(pos.clone(), (pos, len)))
    }

    pub fn builtin(s: String) -> Ident {
        Ident(s, NodeInfo::with_only_pos(Position::builtin()))
    }

    pub fn is_internal(&self) -> bool {
        self.1.pos().isInternal()
    }

    pub fn to_string(self) -> String {
        self.0
    }

    // TODO: should this be a Debug impl?
    pub fn dump(&self) -> String {
        format!("{:?} at {:?}", self.0, self.1)
    }
}
