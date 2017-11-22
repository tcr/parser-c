// Original file: "SemError.hs"
// File auto-generated using Corollary.

use data::node::*;
use data::error::*;
use data::position::Pos;
use analysis::sem_rep::*;

#[derive(Debug)]
pub struct InvalidASTError(pub ErrorInfo);

impl Error for InvalidASTError {
    fn errorInfo(&self) -> ErrorInfo { self.0.clone() }
}


#[derive(Debug)]
pub struct BadSpecifierError(pub ErrorInfo);

impl Error for BadSpecifierError {
    fn errorInfo(&self) -> ErrorInfo { self.0.clone() }
}


#[derive(Debug)]
pub struct RedefError(pub ErrorLevel, pub RedefInfo);

impl Error for RedefError {
    fn errorInfo(&self) -> ErrorInfo {
        let msgs = vec![
            self.1.redefErrReason(),
            format!("The previous declaration was here: {}",
                    ((self.1).3).pos()),
        ];
        ErrorInfo(self.0, ((self.1).2).pos().clone(), msgs)
    }
}


#[derive(Debug)]
pub struct RedefInfo(pub String, pub RedefKind, pub NodeInfo, pub NodeInfo);

#[derive(Clone, Copy, Debug)]
pub enum RedefKind {
    DuplicateDef,
    DiffKindRedecl,
    ShadowedDef,
    DisagreeLinkage,
    NoLinkageOld,
}
pub use self::RedefKind::*;

impl RedefInfo {
    fn redefErrReason(&self) -> String {
        match *self {
            RedefInfo(ref ident, DuplicateDef, _, _) => {
                format!("duplicate definition of {}", ident)
            }
            RedefInfo(ref ident, ShadowedDef, _, _) => {
                format!("this declaration of {} shadows a previous one", ident)
            }
            RedefInfo(ref ident, DiffKindRedecl, _, _) => {
                format!("{} previously declared as a different kind of symbol", ident)
            }
            RedefInfo(ref ident, DisagreeLinkage, _, _) => {
                format!("{} previously declared with different linkage", ident)
            }
            RedefInfo(ref ident, NoLinkageOld, _, _) => {
                format!("{} previously declared without linkage", ident)
            }
        }
    }

    fn redefErrLabel(&self) -> String {
        format!("{} redefined", self.0)
    }
}

#[derive(Debug)]
pub struct TypeMismatch(pub String, pub (NodeInfo, Type), pub (NodeInfo, Type));

impl Error for TypeMismatch {
    fn errorInfo(&self) -> ErrorInfo {
        ErrorInfo(LevelError, ((self.1).0).pos().clone(), vec![self.0.clone()])
    }
}


pub fn invalidAST(node_info: NodeInfo, msg: String) -> InvalidASTError {
    InvalidASTError(mkErrorInfo(LevelError, msg, node_info))
}

pub fn badSpecifierError(node_info: NodeInfo, msg: String) -> BadSpecifierError {
    BadSpecifierError(mkErrorInfo(LevelError, msg, node_info))
}

pub fn typeMismatch(s: String, t: (NodeInfo, Type), u: (NodeInfo, Type)) -> TypeMismatch {
    TypeMismatch(s, t, u)
}

pub fn typeMismatchInfo(TypeMismatch(reason, (node1, _ty2), _t2): TypeMismatch) -> ErrorInfo {
    ErrorInfo(LevelError, node1.into_pos(), vec![reason])
}

pub fn redefinition(lvl: ErrorLevel,
                    ctx: String,
                    kind: RedefKind,
                    new: NodeInfo,
                    old: NodeInfo)
                    -> RedefError {
    RedefError(lvl, (RedefInfo(ctx, kind, new, old)))
}
