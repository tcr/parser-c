// Original file: "Ops.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Data::Generics;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum CAssignOp {
    CAssignOp,
    CMulAssOp,
    CDivAssOp,
    CRmdAssOp,
    CAddAssOp,
    CSubAssOp,
    CShlAssOp,
    CShrAssOp,
    CAndAssOp,
    CXorAssOp,
    COrAssOp,
}
pub use self::CAssignOp::*;

pub fn assignBinop(_0: CAssignOp) -> CBinaryOp {
    match (_0) {
        CAssignOp => panic!("direct assignment has no binary operator"),
        CMulAssOp => CMulOp,
        CDivAssOp => CDivOp,
        CRmdAssOp => CRmdOp,
        CAddAssOp => CAddOp,
        CSubAssOp => CSubOp,
        CShlAssOp => CShlOp,
        CShrAssOp => CShrOp,
        CAndAssOp => CAndOp,
        CXorAssOp => CXorOp,
        COrAssOp => COrOp,
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum CBinaryOp {
    CMulOp,
    CDivOp,
    CRmdOp,
    CAddOp,
    CSubOp,
    CShlOp,
    CShrOp,
    CLeOp,
    CGrOp,
    CLeqOp,
    CGeqOp,
    CEqOp,
    CNeqOp,
    CAndOp,
    CXorOp,
    COrOp,
    CLndOp,
    CLorOp,
}
pub use self::CBinaryOp::*;

pub fn isCmpOp(op: CBinaryOp) -> bool {
    [CLeqOp, CGeqOp, CLeOp, CGrOp, CEqOp, CNeqOp].contains(&op)
}

pub fn isPtrOp(op: CBinaryOp) -> bool {
    [CAddOp, CSubOp].contains(&op)
}

pub fn isBitOp(op: CBinaryOp) -> bool {
    [CShlOp, CShrOp, CAndOp, COrOp, CXorOp].contains(&op)
}

pub fn isLogicOp(op: CBinaryOp) -> bool {
    [CLndOp, CLorOp].contains(&op)
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum CUnaryOp {
    CPreIncOp,
    CPreDecOp,
    CPostIncOp,
    CPostDecOp,
    CAdrOp,
    CIndOp,
    CPlusOp,
    CMinOp,
    CCompOp,
    CNegOp,
}
pub use self::CUnaryOp::*;

pub fn isEffectfulOp(op: CUnaryOp) -> bool {
    [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp].contains(&op)
}
