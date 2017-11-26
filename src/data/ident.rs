// Original file: "Ident.hs"
// File auto-generated using Corollary.

use std::fmt;
use std::rc::Rc;
use std::hash::{Hash, Hasher};

use data::position::{Position, Pos};
use data::node::{NodeInfo, CNode};
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

#[derive(Debug, PartialOrd, Eq)]
struct RawIdent(pub String, pub NodeInfo);

// required because we keep Idents in a HashSet and don't want the set to
// consider the NodeInfo part important for comparison
impl Hash for RawIdent {
    fn hash<H: Hasher>(&self, h: &mut H) {
        (self.0).hash(h);
    }
}

// the definition of the equality allows identifiers to be equal that are
// defined at different source text positions
impl PartialEq for RawIdent {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Clone, PartialOrd, PartialEq, Eq, Hash)]
pub struct Ident(Rc<RawIdent>);

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ident({:?}, {:?})", (self.0).0, (self.0).1)
    }
}

// -- identifiers are attributed
impl CNode for Ident {
    fn node_info(&self) -> &NodeInfo {
        &(self.0).1
    }
    fn into_node_info(self) -> NodeInfo {
        (self.0).1.clone()
    }
}

impl Ident {
    pub fn new(pos: Rc<Position>, s: String, name: Name) -> Ident {
        let len = s.len();
        Ident(Rc::new(RawIdent(s, NodeInfo::new(pos.clone(), pos, len, name))))
    }

    pub fn internal(s: String) -> Ident {
        Ident(Rc::new(RawIdent(s, NodeInfo::with_only_pos(Position::internal()))))
    }

    pub fn internal_at(pos: Position, s: String) -> Ident {
        let len = s.len();
        Ident(Rc::new(RawIdent(s, NodeInfo::with_pos_len(pos.clone(), pos, len))))
    }

    pub fn builtin(s: String) -> Ident {
        Ident(Rc::new(RawIdent(s, NodeInfo::with_only_pos(Position::builtin()))))
    }

    pub fn is_internal(&self) -> bool {
        (self.0).1.pos().isInternal()
    }

    pub fn to_string(&self) -> String {
        (self.0).0.clone()
    }

    pub fn as_str(&self) -> &str {
        &(self.0).0
    }

    // TODO: should this be a Debug impl?
    pub fn dump(&self) -> String {
        format!("{:?} at {:?}", (self.0).0, (self.0).1)
    }
}
