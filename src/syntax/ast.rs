// Original file: "AST.hs"
// File auto-generated using Corollary.

use either::Either;

use data::node::{NodeInfo, CNode};
use data::ident::Ident;
use syntax::ops::*;
use syntax::constants::*;

pub type CTranslUnit = CTranslationUnit<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub struct CTranslationUnit<I>(pub Vec<CExternalDeclaration<I>>, pub I);

pub type CExtDecl = CExternalDeclaration<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CExternalDeclaration<I> {
    CDeclExt(CDeclaration<I>),
    CFDefExt(CFunctionDef<I>),
    CAsmExt(CStringLiteral<I>, I),
}
pub use self::CExternalDeclaration::*;

pub type CFunDef = CFunctionDef<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub struct CFunctionDef<I>(pub Vec<CDeclarationSpecifier<I>>,
                           pub CDeclarator<I>,
                           pub Vec<CDeclaration<I>>,
                           pub CStatement<I>,
                           pub I);


pub type CDecl = CDeclaration<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CDeclaration<I> {
    CDecl(Vec<CDeclarationSpecifier<I>>,
          Vec<(Option<CDeclarator<I>>, Option<CInitializer<I>>, Option<CExpression<I>>)>,
          I),
    CStaticAssert(CExpression<I>, CStringLiteral<I>, I),
}
pub use self::CDeclaration::*;

pub type CDeclr = CDeclarator<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub struct CDeclarator<I>(pub Option<Ident>,
                          pub Vec<CDerivedDeclarator<I>>,
                          pub Option<CStringLiteral<I>>,
                          pub Vec<CAttribute<I>>,
                          pub I);


pub type CDerivedDeclr = CDerivedDeclarator<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CDerivedDeclarator<I> {
    CPtrDeclr(Vec<CTypeQualifier<I>>, I),
    CArrDeclr(Vec<CTypeQualifier<I>>, CArraySize<I>, I),
    CFunDeclr(Either<Vec<Ident>, (Vec<CDeclaration<I>>, bool)>, Vec<CAttribute<I>>, I),
}
pub use self::CDerivedDeclarator::*;

pub type CArrSize = CArraySize<NodeInfo>;

#[derive(Clone, Debug)]
pub enum CArraySize<I> {
    CNoArrSize(bool),
    CArrSize(bool, CExpression<I>),
}
pub use self::CArraySize::*;

pub type CStat = CStatement<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CStatement<I> {
    CLabel(Ident, Box<CStatement<I>>, Vec<CAttribute<I>>, I),
    CCase(CExpression<I>, Box<CStatement<I>>, I),
    CCases(CExpression<I>, CExpression<I>, Box<CStatement<I>>, I),
    CDefault(Box<CStatement<I>>, I),
    CExpr(Option<CExpression<I>>, I),
    CCompound(Vec<Ident>, Vec<CCompoundBlockItem<I>>, I),
    CIf(CExpression<I>, Box<CStatement<I>>, Option<Box<CStatement<I>>>, I),
    CSwitch(CExpression<I>, Box<CStatement<I>>, I),
    CWhile(CExpression<I>, Box<CStatement<I>>, bool, I),
    CFor(Either<Option<CExpression<I>>, CDeclaration<I>>,
         Option<CExpression<I>>,
         Option<CExpression<I>>,
         Box<CStatement<I>>,
         I),
    CGoto(Ident, I),
    CGotoPtr(CExpression<I>, I),
    CCont(I),
    CBreak(I),
    CReturn(Option<CExpression<I>>, I),
    CAsm(CAssemblyStatement<I>, I),
}
pub use self::CStatement::*;

pub type CAsmStmt = CAssemblyStatement<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub struct CAssemblyStatement<I>(pub Option<CTypeQualifier<I>>,
                                 pub CStringLiteral<I>,
                                 pub Vec<CAssemblyOperand<I>>,
                                 pub Vec<CAssemblyOperand<I>>,
                                 pub Vec<CStringLiteral<I>>,
                                 pub I);


pub type CAsmOperand = CAssemblyOperand<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub struct CAssemblyOperand<I>(pub Option<Ident>, pub CStringLiteral<I>, pub CExpression<I>, pub I);


pub type CBlockItem = CCompoundBlockItem<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CCompoundBlockItem<I> {
    CBlockStmt(CStatement<I>),
    CBlockDecl(CDeclaration<I>),
    CNestedFunDef(CFunctionDef<I>),
}
pub use self::CCompoundBlockItem::*;

pub type CDeclSpec = CDeclarationSpecifier<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CDeclarationSpecifier<I> {
    CStorageSpec(CStorageSpecifier<I>),
    CTypeSpec(CTypeSpecifier<I>),
    CTypeQual(CTypeQualifier<I>),
    CFunSpec(CFunctionSpecifier<I>),
    CAlignSpec(CAlignmentSpecifier<I>),
}
pub use self::CDeclarationSpecifier::*;

pub fn partitionDeclSpecs<I>(input: Vec<CDeclarationSpecifier<I>>)
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
            CTypeQual(CAttrQual(attr)) => attrqual.push(attr),
            CTypeQual(tq) => typequal.push(tq),
            CTypeSpec(ts) => typespec.push(ts),
            CFunSpec(fs) => funspec.push(fs),
            CAlignSpec(asp) => alignspec.push(asp),
        }
    }

    (storage, attrqual, typequal, typespec, funspec, alignspec)
}

pub type CStorageSpec = CStorageSpecifier<NodeInfo>;

#[derive(Clone, Debug, CNode, Eq, Ord, PartialEq, PartialOrd)]
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

#[derive(Clone, Debug, CNode)]
pub enum CTypeSpecifier<I> {
    CVoidType(I),
    CCharType(I),
    CShortType(I),
    CIntType(I),
    CLongType(I),
    CFloatType(I),
    CDoubleType(I),
    CSignedType(I),
    CUnsigType(I),
    CBoolType(I),
    CComplexType(I),
    CInt128Type(I),
    CSUType(CStructureUnion<I>, I),
    CEnumType(CEnumeration<I>, I),
    CTypeDef(Ident, I),
    CTypeOfExpr(CExpression<I>, I),
    CTypeOfType(CDeclaration<I>, I),
    CAtomicType(CDeclaration<I>, I),
}
pub use self::CTypeSpecifier::*;

pub fn isSUEDef<I>(spec: CTypeSpecifier<I>) -> bool {
    match spec {
        CSUType(CStructureUnion(_, _, Some(_), _, _), _) => true,
        CEnumType(CEnumeration(_, Some(_), _, _), _) => true,
        _ => true,
    }
}

pub type CTypeQual = CTypeQualifier<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CTypeQualifier<I> {
    CConstQual(I),
    CVolatQual(I),
    CRestrQual(I),
    CAtomicQual(I),
    CAttrQual(CAttribute<I>),
    CNullableQual(I),
    CNonnullQual(I),
}
pub use self::CTypeQualifier::*;

pub type CFunSpec = CFunctionSpecifier<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CFunctionSpecifier<I> {
    CInlineQual(I),
    CNoreturnQual(I),
}
pub use self::CFunctionSpecifier::*;

pub type CAlignSpec = CAlignmentSpecifier<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CAlignmentSpecifier<I> {
    CAlignAsType(CDeclaration<I>, I),
    CAlignAsExpr(CExpression<I>, I),
}
pub use self::CAlignmentSpecifier::*;

pub type CStructUnion = CStructureUnion<NodeInfo>;

#[derive(Clone, Debug, CNode)]
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

#[derive(Clone, Debug, CNode)]
pub struct CEnumeration<I>(pub Option<Ident>,
                           pub Option<Vec<(Ident, Option<CExpression<I>>)>>,
                           pub Vec<CAttribute<I>>,
                           pub I);


pub type CInit = CInitializer<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CInitializer<I> {
    CInitExpr(CExpression<I>, I),
    CInitList(CInitializerList<I>, I),
}
pub use self::CInitializer::*;

pub fn fmapInitList<A, B, F>(_f: F, _initlist: CInitializerList<A>) -> CInitializerList<B>
    where F: Fn(A) -> B
{
    // initlist.into_iter().map(|(desigs, initializer)| {
    //     (desigs.fmap(|desig| desig.fmap(f)), initializer.fmap(f))
    // }).collect()
    // TODO this needs all the Functor instances
    unimplemented!()
}

pub type CInitList = CInitializerList<NodeInfo>;

pub type CInitializerList<I> = Vec<(Vec<CPartDesignator<I>>, CInitializer<I>)>;

pub type CDesignator = CPartDesignator<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CPartDesignator<I> {
    CArrDesig(CExpression<I>, I),
    CMemberDesig(Ident, I),
    CRangeDesig(CExpression<I>, CExpression<I>, I),
}
pub use self::CPartDesignator::*;

pub type CAttr = CAttribute<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub struct CAttribute<I>(pub Ident, pub Vec<CExpression<I>>, pub I);


pub type CExpr = CExpression<NodeInfo>;

#[derive(Clone, Debug, CNode)]
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
    CConst(CConstant<I>),
    CCompoundLit(Box<CDeclaration<I>>, CInitializerList<I>, I),
    CGenericSelection(Box<CExpression<I>>, Vec<(Option<CDeclaration<I>>, CExpression<I>)>, I),
    CStatExpr(Box<CStatement<I>>, I),
    CLabAddrExpr(Ident, I),
    CBuiltinExpr(Box<CBuiltinThing<I>>),
}
pub use self::CExpression::*;

pub type CBuiltin = CBuiltinThing<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CBuiltinThing<I> {
    CBuiltinVaArg(CExpression<I>, CDeclaration<I>, I),
    CBuiltinOffsetOf(CDeclaration<I>, Vec<CPartDesignator<I>>, I),
    CBuiltinTypesCompatible(CDeclaration<I>, CDeclaration<I>, I),
}
pub use self::CBuiltinThing::*;

pub type CConst = CConstant<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub enum CConstant<I> {
    CIntConst(CInteger, I),
    CCharConst(CChar, I),
    CFloatConst(CFloat, I),
    CStrConst(CString, I),
}
pub use self::CConstant::*;

pub type CStrLit = CStringLiteral<NodeInfo>;

#[derive(Clone, Debug, CNode)]
pub struct CStringLiteral<I>(pub CString, pub I);


pub fn cstringOfLit<I>(CStringLiteral(cstr, _): CStringLiteral<I>) -> CString {
    cstr
}

pub fn liftStrLit<I>(CStringLiteral(__str, at): CStringLiteral<I>) -> CConstant<I> {
    CStrConst(__str, at)
}
