// Original file: "SemRep.hs"
// File auto-generated using Corollary.

use std::collections::BTreeMap;

use data::ident::*;
use data::node::*;
use syntax::ast::*;

pub trait HasCompTyKind {
    fn compTag(&self) -> CompTyKind;
}

pub trait HasSUERef {
    fn sueRef(&self) -> SUERef;
}

pub trait Declaration {
    fn getVarDecl(&self) -> VarDecl;
}

#[derive(Clone, Debug)]
pub enum TagDef {
    CompDef(CompType),
    EnumDef(EnumType),
}
pub use self::TagDef::*;

impl HasSUERef for TagDef {
    fn sueRef(&self) -> SUERef {
        match *self {
            CompDef(ref ct) => ct.sueRef(),
            EnumDef(ref et) => et.sueRef(),
        }
    }
}

pub fn typeOfTagDef(def: TagDef) -> TypeName {
    match def {
        CompDef(comptype) => typeOfCompDef(comptype),
        EnumDef(enumtype) => typeOfEnumDef(enumtype),
    }
}

pub fn declOfDef<N: Declaration + CNode + Clone>(def: &N) -> Decl {
    let vd = def.getVarDecl();

    Decl(vd, def.clone().into_node_info())
}

pub fn declIdent<N: Declaration>(def: &N) -> Ident {
    identOfVarName(declName(def))
}

pub fn declName<N: Declaration>(def: &N) -> VarName {
    def.getVarDecl().0
}

pub fn declType<N: Declaration>(def: &N) -> Type {
    def.getVarDecl().2
}

pub fn declAttrs<N: Declaration>(def: &N) -> DeclAttrs {
    def.getVarDecl().1
}

#[derive(Clone, Debug)]
pub enum IdentDecl {
    Declaration(Decl),
    ObjectDef(ObjDef),
    FunctionDef(FunDef),
    EnumeratorDef(Enumerator),
}
pub use self::IdentDecl::*;

impl Declaration for IdentDecl {
    fn getVarDecl(&self) -> VarDecl {
        match *self {
            Declaration(ref decl) => decl.getVarDecl(),
            ObjectDef(ref def) => def.getVarDecl(),
            FunctionDef(ref def) => def.getVarDecl(),
            EnumeratorDef(ref def) => def.getVarDecl(),
        }
    }
}

pub fn objKindDescr(decl: &IdentDecl) -> String {
    match *decl {
        Declaration(_) => "declaration",
        ObjectDef(_) => "object definition",
        FunctionDef(_) => "function definition",
        EnumeratorDef(_) => "enumerator definition",
    }.into()
}

pub fn splitIdentDecls(include_all: bool, all: BTreeMap<Ident, IdentDecl>)
                       -> (BTreeMap<Ident, Decl>,
                           (BTreeMap<Ident, Enumerator>, BTreeMap<Ident, ObjDef>, BTreeMap<Ident, FunDef>))
{
    let mut decls = BTreeMap::new();
    let mut enums = BTreeMap::new();
    let mut objs  = BTreeMap::new();
    let mut funcs = BTreeMap::new();

    {
        let mut addDef = |ident, entry| match entry {
            Declaration(_) => {}
            EnumeratorDef(e) => { enums.insert(ident, e); }
            ObjectDef(o) => { objs.insert(ident, o); }
            FunctionDef(f) => { funcs.insert(ident, f); }
        };

        for (ident, entry) in all {
            if include_all {
                decls.insert(ident.clone(), declOfDef(&entry));
                addDef(ident, entry);
            } else {
                match entry {
                    Declaration(d) => { decls.insert(ident, d); }
                    other => { addDef(ident, other); }
                }
            }
        }
    }
    (decls, (enums, objs, funcs))
}

pub struct GlobalDecls {
    pub gObjs: BTreeMap<Ident, IdentDecl>,
    pub gTags: BTreeMap<SUERef, TagDef>,
    pub gTypeDefs: BTreeMap<Ident, TypeDef>,
}

pub fn emptyGlobalDecls() -> GlobalDecls {
    GlobalDecls { gObjs: BTreeMap::new(), gTags: BTreeMap::new(), gTypeDefs: BTreeMap::new() }
}

pub fn filterGlobalDecls<F>(decl_filter: F, gmap: GlobalDecls) -> GlobalDecls
    where F: Fn(DeclEvent) -> bool
{
    GlobalDecls {
        gObjs: gmap.gObjs.into_iter().filter(|&(_, ref v)| decl_filter(DeclEvent(v.clone()))).collect(),
        gTags: gmap.gTags.into_iter().filter(|&(_, ref v)| decl_filter(TagEvent(v.clone()))).collect(),
        gTypeDefs: gmap.gTypeDefs.into_iter().filter(|&(_, ref v)| decl_filter(TypeDefEvent(v.clone()))).collect(),
    }
}

pub fn mergeGlobalDecls(mut gmap1: GlobalDecls, mut gmap2: GlobalDecls) -> GlobalDecls {
    gmap1.gObjs.append(&mut gmap2.gObjs);
    gmap1.gTags.append(&mut gmap2.gTags);
    gmap1.gTypeDefs.append(&mut gmap2.gTypeDefs);
    gmap1
}

pub enum DeclEvent {
    TagEvent(TagDef),
    DeclEvent(IdentDecl),
    ParamEvent(ParamDecl),
    LocalEvent(IdentDecl),
    TypeDefEvent(TypeDef),
    AsmEvent(AsmBlock),
}
pub use self::DeclEvent::*;

#[derive(Clone, Debug)]
pub struct Decl(pub VarDecl, pub NodeInfo);

impl Declaration for Decl {
    fn getVarDecl(&self) -> VarDecl {
        self.0.clone()
    }
}


#[derive(Clone, Debug)]
pub struct ObjDef(pub VarDecl, pub Option<Initializer>, pub NodeInfo);

impl Declaration for ObjDef {
    fn getVarDecl(&self) -> VarDecl {
        self.0.clone()
    }
}

impl ObjDef {
    pub fn isTentative(&self) -> bool {
        if isExtDecl(&self.0) { self.1.is_none() } else { false }
    }
}


#[derive(Clone, Debug)]
pub struct FunDef(pub VarDecl, pub Stmt, pub NodeInfo);

impl Declaration for FunDef {
    fn getVarDecl(&self) -> VarDecl {
        self.0.clone()
    }
}


#[derive(Clone, Debug)]
pub enum ParamDecl {
    ParamDecl(VarDecl, NodeInfo),
    AbstractParamDecl(VarDecl, NodeInfo),
}
pub use self::ParamDecl::*;

impl Declaration for ParamDecl {
    fn getVarDecl(&self) -> VarDecl {
        match *self {
            ParamDecl(ref vd, _) => vd.clone(),
            AbstractParamDecl(ref vd, _) => vd.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum MemberDecl {
    MemberDecl(VarDecl, Option<Expr>, NodeInfo),
    AnonBitField(Type, Expr, NodeInfo),
}
pub use self::MemberDecl::*;

impl Declaration for MemberDecl {
    fn getVarDecl(&self) -> VarDecl {
        match *self {
            MemberDecl(ref vd, ..) => vd.clone(),
            AnonBitField(ref ty, ..) => VarDecl(
                VarName::NoName,
                DeclAttrs(noFunctionAttrs(), NoStorage, noAttributes()),
                ty.clone())
        }
    }
}


#[derive(Clone, Debug)]
pub struct TypeDef(pub Ident, pub Type, pub Attributes, pub NodeInfo);


pub fn identOfTypeDef(&TypeDef(ref ide, _, _, _): &TypeDef) -> Ident {
    ide.clone()
}

#[derive(Clone, Debug)]
pub struct VarDecl(pub VarName, pub DeclAttrs, pub Type);

impl Declaration for VarDecl {
    fn getVarDecl(&self) -> VarDecl {
        self.clone()
    }
}


pub fn isExtDecl<D: Declaration>(decl: &D) -> bool {
    declStorage(decl).hasLinkage()
}

#[derive(Clone, Debug)]
pub struct DeclAttrs(pub FunctionAttrs, pub Storage, pub Attributes);


pub fn declStorage<D: Declaration>(d: &D) -> Storage {
    declAttrs(d).1
}

pub fn functionAttrs<D: Declaration>(d: &D) -> FunctionAttrs {
    declAttrs(d).0
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FunctionAttrs {
    isInline: bool,
    isNoreturn: bool,
}

pub fn noFunctionAttrs() -> FunctionAttrs {
    FunctionAttrs {
        isInline: false,
        isNoreturn: false,
    }
}

pub type ThreadLocal = bool;

pub type Register = bool;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Linkage {
    NoLinkage,
    InternalLinkage,
    ExternalLinkage,
}
pub use self::Linkage::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Storage {
    NoStorage,
    Auto(Register),
    Static(Linkage, ThreadLocal),
    FunLinkage(Linkage),
}
pub use self::Storage::*;

impl Storage {
    pub fn hasLinkage(self) -> bool {
        match self {
            Auto(_) => false,
            Static(NoLinkage, _) => false,
            _ => true,
        }
    }
}

pub fn declLinkage<D: Declaration>(decl: &D) -> Linkage {
    match declStorage(decl) {
        NoStorage => panic!("undefined"),
        Auto(_) => NoLinkage,
        Static(linkage, _) => linkage,
        FunLinkage(linkage) => linkage,
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    DirectType(TypeName, TypeQuals, Attributes),
    PtrType(Box<Type>, TypeQuals, Attributes),
    ArrayType(Box<Type>, ArraySize, TypeQuals, Attributes),
    FunctionType(Box<FunType>, Attributes),
    TypeDefType(Box<TypeDefRef>, TypeQuals, Attributes),
}
pub use self::Type::*;

#[derive(Clone, Debug)]
pub enum FunType {
    FunType(Type, Vec<ParamDecl>, bool),
    FunTypeIncomplete(Type),
}
pub use self::FunType::*;

#[derive(Clone, Debug)]
pub enum ArraySize {
    UnknownArraySize(bool),
    ArraySize(bool, Expr),
}
pub use self::ArraySize::*;

#[derive(Clone, Debug)]
pub enum TypeName {
    TyVoid,
    TyIntegral(IntType),
    TyFloating(FloatType),
    TyComplex(FloatType),
    TyComp(CompTypeRef),
    TyEnum(EnumTypeRef),
    TyBuiltin(BuiltinType),
}
pub use self::TypeName::*;

#[derive(Clone, Copy, Debug)]
pub enum BuiltinType {
    TyVaList,
    TyAny,
}
pub use self::BuiltinType::*;

#[derive(Clone, Debug)]
pub struct TypeDefRef(pub Ident, pub Type, pub NodeInfo);


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntType {
    TyBool,
    TyChar,
    TySChar,
    TyUChar,
    TyShort,
    TyUShort,
    TyInt,
    TyUInt,
    TyInt128,
    TyUInt128,
    TyLong,
    TyULong,
    TyLLong,
    TyULLong,
}
pub use self::IntType::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatType {
    TyFloat,
    TyDouble,
    TyLDouble,
}
pub use self::FloatType::*;

#[derive(Clone, Debug)]
pub struct CompTypeRef(pub SUERef, pub CompTyKind, pub NodeInfo);

impl HasCompTyKind for CompTypeRef {
    fn compTag(&self) -> CompTyKind {
        self.1
    }
}

impl HasSUERef for CompTypeRef {
    fn sueRef(&self) -> SUERef {
        self.0.clone()
    }
}


#[derive(Clone, Debug)]
pub struct EnumTypeRef(pub SUERef, pub NodeInfo);

impl HasSUERef for EnumTypeRef {
    fn sueRef(&self) -> SUERef {
        self.0.clone()
    }
}


#[derive(Clone, Debug)]
pub struct CompType(pub SUERef, pub CompTyKind, pub Vec<MemberDecl>, pub Attributes, pub NodeInfo);

impl HasCompTyKind for CompType {
    fn compTag(&self) -> CompTyKind {
        self.1
    }
}

impl HasSUERef for CompType {
    fn sueRef(&self) -> SUERef {
        self.0.clone()
    }
}


pub fn typeOfCompDef(CompType(ref_, tag, _, _, _): CompType) -> TypeName {
    TyComp(CompTypeRef(ref_, tag, NodeInfo::undef()))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompTyKind {
    StructTag,
    UnionTag,
}
pub use self::CompTyKind::*;


#[derive(Clone, Debug)]
pub struct EnumType(pub SUERef, pub Vec<Enumerator>, pub Attributes, pub NodeInfo);

impl HasSUERef for EnumType {
    fn sueRef(&self) -> SUERef {
        self.0.clone()
    }
}


pub fn typeOfEnumDef(EnumType(ref_, _, _, _): EnumType) -> TypeName {
    TyEnum(EnumTypeRef(ref_, NodeInfo::undef()))
}

#[derive(Clone, Debug)]
pub struct Enumerator(pub Ident, pub Expr, pub EnumType, pub NodeInfo);

impl Declaration for Enumerator {
    fn getVarDecl(&self) -> VarDecl {
        VarDecl(VarName::VarName(self.0.clone(), None),
                DeclAttrs(noFunctionAttrs(), NoStorage, noAttributes()),
                DirectType(typeOfEnumDef(self.2.clone()), noTypeQuals(), noAttributes()))
    }
}


#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TypeQuals {
    pub constant: bool,
    pub volatile: bool,
    pub restrict: bool,
    pub atomic: bool,
    pub nullable: bool,
    pub nonnull: bool,
}

pub fn noTypeQuals() -> TypeQuals {
    TypeQuals::default()
}

pub fn mergeTypeQuals(t1: TypeQuals, t2: TypeQuals) -> TypeQuals {
    TypeQuals {
        constant: t1.constant && t2.constant,
        volatile: t1.volatile && t2.volatile,
        restrict: t1.restrict && t2.restrict,
        atomic:   t1.atomic   && t2.atomic,
        nullable: t1.nullable && t2.nullable,
        nonnull:  t1.nonnull  && t2.nonnull,
    }
}

pub type Initializer = CInit;

#[derive(Clone, Debug)]
pub enum VarName {
    VarName(Ident, Option<AsmName>),
    NoName,
}
pub use self::VarName::*;

pub fn identOfVarName(name: VarName) -> Ident {
    match name {
        NoName => panic!("identOfVarName: NoName"),
        VarName(ident, _) => ident,
    }
}

pub fn isNoName(name: VarName) -> bool {
    match name {
        NoName => true,
        _ => false,
    }
}

pub type AsmBlock = CStrLit;

pub type AsmName = CStrLit;

#[derive(Clone, Debug)]
pub struct Attr(pub Ident, pub Vec<Expr>, pub NodeInfo);


pub type Attributes = Vec<Attr>;

pub fn noAttributes() -> Attributes {
    vec![]
}

pub fn mergeAttributes(mut a1: Attributes, mut a2: Attributes) -> Attributes {
    a1.append(&mut a2);
    a1
}

pub type Stmt = CStat;

pub type Expr = CExpr;


macro_rules! implement_cnode_pos {
    (struct $ty:ident) => {
        impl CNode for $ty {
            fn node_info(&self) -> &NodeInfo {
                let $ty(.., ref ni) = *self; ni
            }
            fn into_node_info(self) -> NodeInfo {
                let $ty(.., ni) = self; ni
            }
        }
    };
    (enum $ty:ident, $($variant:ident),+) => {
        impl CNode for $ty {
            fn node_info(&self) -> &NodeInfo {
                match *self {
                    $( $variant(.., ref obj) => obj.node_info() ),*
                }
            }
            fn into_node_info(self) -> NodeInfo {
                match self {
                    $( $variant(.., obj) => obj.into_node_info() ),*
                }
            }
        }
    };
}

implement_cnode_pos!(enum TagDef, CompDef, EnumDef);
implement_cnode_pos!(enum IdentDecl, Declaration, ObjectDef, FunctionDef, EnumeratorDef);
implement_cnode_pos!(enum DeclEvent, TagEvent, DeclEvent, ParamEvent, LocalEvent,
                     TypeDefEvent, AsmEvent);
implement_cnode_pos!(struct Decl);
implement_cnode_pos!(struct ObjDef);
implement_cnode_pos!(struct FunDef);
implement_cnode_pos!(enum ParamDecl, ParamDecl, AbstractParamDecl);
implement_cnode_pos!(enum MemberDecl, MemberDecl, AnonBitField);
implement_cnode_pos!(struct TypeDef);
implement_cnode_pos!(struct TypeDefRef);
implement_cnode_pos!(struct CompTypeRef);
implement_cnode_pos!(struct EnumTypeRef);
implement_cnode_pos!(struct CompType);
implement_cnode_pos!(struct EnumType);
implement_cnode_pos!(struct Enumerator);
implement_cnode_pos!(struct Attr);
