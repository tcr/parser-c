// Original file: "AST.hs"
// File auto-generated using Corollary.

use std::any::Any;
use either::Either;

use data::node::{NodeInfo, CNode};
use data::ident::Ident;
use syntax::ops::*;
use syntax::constants::*;

// Trait to compare nodes for equivalence (not equality, since the NodeInfo
// parameter is not compared)

pub trait Equiv {
    fn equiv(&self, other: &Self) -> bool;
}

// Leaf type implementations

macro_rules! equiv_from_eq {
    () => {};
    ($ty:ty, $($tt:tt)*) => {
        impl Equiv for $ty {
            fn equiv(&self, other: &Self) -> bool {
                self == other
            }
        }
        equiv_from_eq!($($tt)*);
    };
}

equiv_from_eq!(bool, CString, CFloat, CChar, CInteger,
               CStructTag, CAssignOp, CBinaryOp, CUnaryOp, );

impl Equiv for Ident {
    fn equiv(&self, other: &Self) -> bool {
        self.as_str() == other.as_str() // only compare string, not nodeinfo
    }
}

// Container implementations

impl<T: Equiv> Equiv for Vec<T> {
    fn equiv(&self, other: &Self) -> bool {
        self.iter().zip(other).all(|(x, y)| x.equiv(y))
    }
}

impl<T: Equiv> Equiv for Option<T> {
    fn equiv(&self, other: &Self) -> bool {
        match (self, other) {
            (&None, &None) => true,
            (&Some(ref a), &Some(ref b)) => a.equiv(b),
            _ => false,
        }
    }
}

impl<T: Equiv, U: Equiv> Equiv for Either<T, U> {
    fn equiv(&self, other: &Self) -> bool {
        match (self, other) {
            (&Either::Left(ref a), &Either::Left(ref b)) => a.equiv(b),
            (&Either::Right(ref a), &Either::Right(ref b)) => a.equiv(b),
            _ => false,
        }
    }
}

impl<T: Equiv, U: Equiv> Equiv for (T, U) {
    fn equiv(&self, other: &Self) -> bool {
        self.0.equiv(&other.0) && self.1.equiv(&other.1)
    }
}

impl<T: Equiv, U: Equiv, V: Equiv> Equiv for (T, U, V) {
    fn equiv(&self, other: &Self) -> bool {
        self.0.equiv(&other.0) && self.1.equiv(&other.1) && self.2.equiv(&other.2)
    }
}

impl<T: Equiv> Equiv for Box<T> {
    fn equiv(&self, other: &Self) -> bool {
        (**self).equiv(&*other)
    }
}

// Functor trait for our nodes, to map the info tag

pub trait NodeFunctor<A, B> {
    type Output;
    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output where Self: Sized;
}

// Generic node traversal trait, visits all nodes with the same function
// that can downcast from &mut Any to different types

pub trait Traverse {
    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F);
}

// Helper instances that lift the fmap and traverse operations

impl<A, B, T: NodeFunctor<A, B>> NodeFunctor<A, B> for Box<T> {
    type Output = Box<T::Output>;
    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
        box (*self).fmap(f)
    }
}

impl<T: Traverse> Traverse for Box<T> {
    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
        (**self).traverse(f)
    }
}

impl<A, B, T: NodeFunctor<A, B>> NodeFunctor<A, B> for Option<T> {
    type Output = Option<T::Output>;
    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
        self.map(|x| x.fmap(f))
    }
}

impl<T: Traverse> Traverse for Option<T> {
    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
        if let Some(ref mut x) = *self { x.traverse(f); }
    }
}

impl<A, B, T: NodeFunctor<A, B>> NodeFunctor<A, B> for Vec<T> {
    type Output = Vec<T::Output>;
    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
        self.into_iter().map(|x| x.fmap(f)).collect()
    }
}

impl<T: Traverse> Traverse for Vec<T> {
    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
        self.iter_mut().for_each(|x| x.traverse(f));
    }
}

impl<A, B, T: NodeFunctor<A, B>, U: NodeFunctor<A, B>> NodeFunctor<A, B> for (T, U) {
    type Output = (T::Output, U::Output);
    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
        (self.0.fmap(f), self.1.fmap(f))
    }
}

impl<T: Traverse, U: Traverse> Traverse for (T, U) {
    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
        self.0.traverse(f);
        self.1.traverse(f);
    }
}

impl<A, B, T: NodeFunctor<A, B>, U: NodeFunctor<A, B>, V: NodeFunctor<A, B>> NodeFunctor<A, B> for (T, U, V) {
    type Output = (T::Output, U::Output, V::Output);
    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
        (self.0.fmap(f), self.1.fmap(f), self.2.fmap(f))
    }
}

impl<T: Traverse, U: Traverse, V: Traverse> Traverse for (T, U, V) {
    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
        self.0.traverse(f);
        self.1.traverse(f);
        self.2.traverse(f);
    }
}

impl<A, B, T: NodeFunctor<A, B>, U: NodeFunctor<A, B>> NodeFunctor<A, B> for Either<T, U> {
    type Output = Either<T::Output, U::Output>;
    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
        self.map_left(|x| x.fmap(f)).map_right(|x| x.fmap(f))
    }
}

impl<T: Traverse, U: Traverse> Traverse for Either<T, U> {
    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
        self.as_mut().map_left(|x| x.traverse(f)).map_right(|x| x.traverse(f));
    }
}

// AST nodes

pub type CTranslUnit = CTranslationUnit<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub struct CTranslationUnit<I>(pub Vec<CExternalDeclaration<I>>, pub I);

pub type CExtDecl = CExternalDeclaration<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CExternalDeclaration<I> {
    CDeclExt(CDeclaration<I>),
    CFDefExt(CFunctionDef<I>),
    CAsmExt(CStringLiteral<I>, I),
}
pub use self::CExternalDeclaration::*;

pub type CFunDef = CFunctionDef<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub struct CFunctionDef<I>(pub Vec<CDeclarationSpecifier<I>>,
                           pub Box<CDeclarator<I>>,
                           pub Vec<CDeclaration<I>>,
                           pub Box<CStatement<I>>,
                           pub I);


pub type CDecl = CDeclaration<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CDeclaration<I> {
    CDecl(Vec<CDeclarationSpecifier<I>>,
          Vec<(Option<Box<CDeclarator<I>>>, Option<Box<CInitializer<I>>>, Option<Box<CExpression<I>>>)>,
          I),
    CStaticAssert(Box<CExpression<I>>, CStringLiteral<I>, I),
}
pub use self::CDeclaration::*;

pub type CDeclr = CDeclarator<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub struct CDeclarator<I>(pub Option<Ident>,
                          pub Vec<CDerivedDeclarator<I>>,
                          pub Option<Box<CStringLiteral<I>>>,
                          pub Vec<CAttribute<I>>,
                          pub I);


pub type CDerivedDeclr = CDerivedDeclarator<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode)]
pub enum CDerivedDeclarator<I> {
    CPtrDeclr(Vec<CTypeQualifier<I>>, I),
    CArrDeclr(Vec<CTypeQualifier<I>>, CArraySize<I>, I),
    CFunDeclr(Either<Vec<Ident>, (Vec<CDeclaration<I>>, bool)>, Vec<CAttribute<I>>, I),
}
pub use self::CDerivedDeclarator::*;

// This one would require dummy instances for Ident and bool, so implement it manually.

impl<A, B> NodeFunctor<A, B> for CDerivedDeclarator<A> {
    type Output = CDerivedDeclarator<B>;
    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
        match self {
            CPtrDeclr(tq, a) => CPtrDeclr(tq.fmap(f), f(a)),
            CArrDeclr(tq, sz, a) => CArrDeclr(tq.fmap(f), sz.fmap(f), f(a)),
            CFunDeclr(id_or_args, attrs, a) =>
                CFunDeclr(id_or_args.map_right(|(decl, x)| (decl.fmap(f), x)),
                          attrs.fmap(f), f(a)),
        }
    }
}
impl Traverse for CDerivedDeclarator<NodeInfo> {
    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
        f(self);
        match *self {
            CPtrDeclr(ref mut tq, _) => { tq.traverse(f); }
            CArrDeclr(ref mut tq, ref mut sz, _) => {
                tq.traverse(f);
                if let CArrSize(_, ref mut expr) = *sz {
                    expr.traverse(f);
                }
            }
            CFunDeclr(ref mut id, ref mut attrs, _) => {
                if let Either::Right(ref mut decls) = *id {
                    decls.0.traverse(f);
                }
                attrs.traverse(f);
            }
        }
    }
}


pub type CArrSize = CArraySize<NodeInfo>;

#[derive(Clone, Debug, Equiv, NodeFunctor)]
pub enum CArraySize<I> {
    CNoArrSize(bool),
    CArrSize(bool, Box<CExpression<I>>),
}
pub use self::CArraySize::*;

pub type CStat = CStatement<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CStatement<I> {
    CLabel(Ident, Box<CStatement<I>>, Vec<CAttribute<I>>, I),
    CCase(Box<CExpression<I>>, Box<CStatement<I>>, I),
    CCases(Box<CExpression<I>>, Box<CExpression<I>>, Box<CStatement<I>>, I),
    CDefault(Box<CStatement<I>>, I),
    CExpr(Option<Box<CExpression<I>>>, I),
    CCompound(Vec<Ident>, Vec<CCompoundBlockItem<I>>, I),
    CIf(Box<CExpression<I>>, Box<CStatement<I>>, Option<Box<CStatement<I>>>, I),
    CSwitch(Box<CExpression<I>>, Box<CStatement<I>>, I),
    CWhile(Box<CExpression<I>>, Box<CStatement<I>>, bool, I),
    CFor(Either<Option<Box<CExpression<I>>>, Box<CDeclaration<I>>>,
         Option<Box<CExpression<I>>>,
         Option<Box<CExpression<I>>>,
         Box<CStatement<I>>,
         I),
    CGoto(Ident, I),
    CGotoPtr(Box<CExpression<I>>, I),
    CCont(I),
    CBreak(I),
    CReturn(Option<Box<CExpression<I>>>, I),
    CAsm(Box<CAssemblyStatement<I>>, I),
}
pub use self::CStatement::*;


pub type CAsmStmt = CAssemblyStatement<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub struct CAssemblyStatement<I>(pub Option<Box<CTypeQualifier<I>>>,
                                 pub Box<CStringLiteral<I>>,
                                 pub Vec<CAssemblyOperand<I>>,
                                 pub Vec<CAssemblyOperand<I>>,
                                 pub Vec<CStringLiteral<I>>,
                                 pub I);


pub type CAsmOperand = CAssemblyOperand<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub struct CAssemblyOperand<I>(pub Option<Ident>, pub Box<CStringLiteral<I>>, pub Box<CExpression<I>>, pub I);


pub type CBlockItem = CCompoundBlockItem<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CCompoundBlockItem<I> {
    CBlockStmt(CStatement<I>),
    CBlockDecl(CDeclaration<I>),
    CNestedFunDef(CFunctionDef<I>),
}
pub use self::CCompoundBlockItem::*;

pub type CDeclSpec = CDeclarationSpecifier<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CDeclarationSpecifier<I> {
    CStorageSpec(CStorageSpecifier<I>),
    CTypeSpec(CTypeSpecifier<I>),
    CTypeQual(CTypeQualifier<I>),
    CFunSpec(CFunctionSpecifier<I>),
    CAlignSpec(CAlignmentSpecifier<I>),
}
pub use self::CDeclarationSpecifier::*;

pub fn partition_declspecs<I>(input: Vec<CDeclarationSpecifier<I>>)
    -> (Vec<CStorageSpecifier<I>>,
        Vec<CAttribute<I>>,
        Vec<CTypeQualifier<I>>,
        Vec<CTypeSpecifier<I>>,
        Vec<CFunctionSpecifier<I>>,
        Vec<CAlignmentSpecifier<I>>)
{
    let mut storage = vec![];
    let mut attrqual = vec![];
    let mut typequal = vec![];
    let mut typespec = vec![];
    let mut funspec = vec![];
    let mut alignspec = vec![];

    for declspec in input {
        match declspec {
            CStorageSpec(sp) => storage.push(sp),
            CTypeQual(CAttrQual(attr)) => attrqual.push(*attr),
            CTypeQual(tq) => typequal.push(tq),
            CTypeSpec(ts) => typespec.push(ts),
            CFunSpec(fs) => funspec.push(fs),
            CAlignSpec(asp) => alignspec.push(asp),
        }
    }

    (storage, attrqual, typequal, typespec, funspec, alignspec)
}

pub type CStorageSpec = CStorageSpecifier<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse, Eq, Ord, PartialEq, PartialOrd)]
pub enum CStorageSpecifier<I> {
    CAuto(I),
    CRegister(I),
    CStatic(I),
    CExtern(I),
    CTypedef(I),
    CThread(I),
}
pub use self::CStorageSpecifier::*;

pub type CTypeSpec = CTypeSpecifier<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CTypeSpecifier<I> {
    CVoidType(I),
    CCharType(I),
    CShortType(I),
    CIntType(I),
    CLongType(I),
    CFloatType(I),
    CFloat128Type(I),
    CDoubleType(I),
    CSignedType(I),
    CUnsigType(I),
    CBoolType(I),
    CComplexType(I),
    CInt128Type(I),
    CSUType(Box<CStructureUnion<I>>, I),
    CEnumType(Box<CEnumeration<I>>, I),
    CTypeDef(Ident, I),
    CTypeOfExpr(Box<CExpression<I>>, I),
    CTypeOfType(Box<CDeclaration<I>>, I),
    CAtomicType(Box<CDeclaration<I>>, I),
}
pub use self::CTypeSpecifier::*;

impl<I> CTypeSpecifier<I> where I: ::std::fmt::Debug {
    pub fn is_sue_def(&self) -> bool {
        match *self {
            CSUType(box CStructureUnion(_, _, Some(_), _, _), _) => true,
            CEnumType(box CEnumeration(_, Some(_), _, _), _) => true,
            _ => false,
        }
    }
}

pub type CTypeQual = CTypeQualifier<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CTypeQualifier<I> {
    CConstQual(I),
    CVolatQual(I),
    CRestrQual(I),
    CAtomicQual(I),
    CAttrQual(Box<CAttribute<I>>),
    CNullableQual(I),
    CNonnullQual(I),
}
pub use self::CTypeQualifier::*;

pub type CFunSpec = CFunctionSpecifier<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CFunctionSpecifier<I> {
    CInlineQual(I),
    CNoreturnQual(I),
}
pub use self::CFunctionSpecifier::*;

pub type CAlignSpec = CAlignmentSpecifier<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CAlignmentSpecifier<I> {
    CAlignAsType(Box<CDeclaration<I>>, I),
    CAlignAsExpr(Box<CExpression<I>>, I),
}
pub use self::CAlignmentSpecifier::*;

pub type CStructUnion = CStructureUnion<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub struct CStructureUnion<I>(pub CStructTag,
                              pub Option<Ident>,
                              pub Option<Vec<CDeclaration<I>>>,
                              pub Vec<CAttribute<I>>,
                              pub I);
pub type CStruct = CStructureUnion<NodeInfo>;


#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CStructTag {
    CStructTag,
    CUnionTag,
}
pub use self::CStructTag::*;

pub type CEnum = CEnumeration<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode)]
pub struct CEnumeration<I>(pub Option<Ident>,
                           pub Option<Vec<(Ident, Option<Box<CExpression<I>>>)>>,
                           pub Vec<CAttribute<I>>,
                           pub I);

// This one would require a dummy instance for Ident, so implement it manually.

impl<A, B> NodeFunctor<A, B> for CEnumeration<A> {
    type Output = CEnumeration<B>;
    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
        let CEnumeration(id, exprs, attrs, a) = self;
        CEnumeration(id,
                     exprs.map(|v| v.into_iter().map(|(eid, expr)| (eid, expr.fmap(f))).collect()),
                     attrs.fmap(f),
                     f(a))
    }
}
impl Traverse for CEnumeration<NodeInfo> {
    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
        f(self);
        if let Some(ref mut exprs) = self.1 {
            exprs.iter_mut().for_each(|v| v.1.traverse(f));
        }
        self.2.traverse(f);
    }
}


pub type CInit = CInitializer<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CInitializer<I> {
    CInitExpr(Box<CExpression<I>>, I),
    CInitList(CInitializerList<I>, I),
}
pub use self::CInitializer::*;

pub type CInitList = CInitializerList<NodeInfo>;

pub type CInitializerList<I> = Vec<(Vec<CPartDesignator<I>>, Box<CInitializer<I>>)>;

pub type CDesignator = CPartDesignator<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CPartDesignator<I> {
    CArrDesig(Box<CExpression<I>>, I),
    CMemberDesig(Ident, I),
    CRangeDesig(Box<CExpression<I>>, Box<CExpression<I>>, I),
}
pub use self::CPartDesignator::*;

pub type CAttr = CAttribute<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub struct CAttribute<I>(pub Ident, pub Vec<CExpression<I>>, pub I);


pub type CExpr = CExpression<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CExpression<I> {
    CComma(Vec<CExpression<I>>, I),
    CAssign(CAssignOp, Box<CExpression<I>>, Box<CExpression<I>>, I),
    CCond(Box<CExpression<I>>, Option<Box<CExpression<I>>>, Box<CExpression<I>>, I),
    CBinary(CBinaryOp, Box<CExpression<I>>, Box<CExpression<I>>, I),
    CCast(Box<CDeclaration<I>>, Box<CExpression<I>>, I),
    CUnary(CUnaryOp, Box<CExpression<I>>, I),
    CSizeofExpr(Box<CExpression<I>>, I),
    CSizeofType(Box<CDeclaration<I>>, I),
    CAlignofExpr(Box<CExpression<I>>, I),
    CAlignofType(Box<CDeclaration<I>>, I),
    CComplexReal(Box<CExpression<I>>, I),
    CComplexImag(Box<CExpression<I>>, I),
    CIndex(Box<CExpression<I>>, Box<CExpression<I>>, I),
    CCall(Box<CExpression<I>>, Vec<CExpression<I>>, I),
    CMember(Box<CExpression<I>>, Ident, bool, I),
    CVar(Ident, I),
    CConst(Box<CConstant<I>>),
    CCompoundLit(Box<CDeclaration<I>>, CInitializerList<I>, I),
    CGenericSelection(Box<CExpression<I>>, Vec<(Option<Box<CDeclaration<I>>>, Box<CExpression<I>>)>, I),
    CStatExpr(Box<CStatement<I>>, I),
    CLabAddrExpr(Ident, I),
    CBuiltinExpr(Box<CBuiltinThing<I>>),
}
pub use self::CExpression::*;

pub type CBuiltin = CBuiltinThing<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CBuiltinThing<I> {
    CBuiltinVaArg(Box<CExpression<I>>, Box<CDeclaration<I>>, I),
    CBuiltinOffsetOf(Box<CDeclaration<I>>, Vec<CPartDesignator<I>>, I),
    CBuiltinTypesCompatible(Box<CDeclaration<I>>, Box<CDeclaration<I>>, I),
    CBuiltinConvertVector(Box<CExpression<I>>, Box<CDeclaration<I>>, I),
}
pub use self::CBuiltinThing::*;

pub type CConst = CConstant<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub enum CConstant<I> {
    CIntConst(CInteger, I),
    CCharConst(CChar, I),
    CFloatConst(CFloat, I),
    CStrConst(CString, I),
}
pub use self::CConstant::*;

impl<I> CConstant<I> {
    pub fn from_strlit(lit: CStringLiteral<I>) -> CConstant<I> {
        CStrConst(lit.0, lit.1)
    }
}

pub type CStrLit = CStringLiteral<NodeInfo>;

#[derive(Clone, Debug, Equiv, CNode, NodeFunctor, Traverse)]
pub struct CStringLiteral<I>(pub CString, pub I);
