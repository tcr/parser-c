// Original file: "Ops.hs"
// File auto-generated using Corollary.

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

impl CAssignOp {
    pub fn binop(&self) -> Option<CBinaryOp> {
        match *self {
            CAssignOp => None,
            CMulAssOp => Some(CMulOp),
            CDivAssOp => Some(CDivOp),
            CRmdAssOp => Some(CRmdOp),
            CAddAssOp => Some(CAddOp),
            CSubAssOp => Some(CSubOp),
            CShlAssOp => Some(CShlOp),
            CShrAssOp => Some(CShrOp),
            CAndAssOp => Some(CAndOp),
            CXorAssOp => Some(CXorOp),
            COrAssOp  => Some(COrOp),
        }
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

impl CBinaryOp {
    pub fn is_cmp_op(&self) -> bool {
        match *self {
            CLeqOp | CGeqOp | CLeOp | CGrOp | CEqOp | CNeqOp => true,
            _ => false,
        }
    }

    pub fn is_ptr_op(&self) -> bool {
        match *self {
            CAddOp | CSubOp => true,
            _ => false,
        }
    }

    pub fn is_bit_op(&self) -> bool {
        match *self {
            CShlOp | CShrOp | CAndOp | COrOp | CXorOp => true,
            _ => false,
        }
    }

    pub fn is_logic_op(&self) -> bool {
        match *self {
            CLndOp | CLorOp => true,
            _ => false,
        }
    }
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

impl CUnaryOp {
    pub fn is_effectful_op(&self) -> bool {
        match *self {
            CPreIncOp | CPreDecOp | CPostIncOp | CPostDecOp => true,
            _ => false,
        }
    }
}
