// Original file: "TypeUtils.hs"
// File auto-generated using Corollary.

use data::node::CNode;
use syntax::ast::{CConstant, CExpression};
use analysis::sem_rep::*;

pub fn integral(ty: IntType) -> Type {
    DirectType(TyIntegral(ty), noTypeQuals(), noAttributes())
}

pub fn floating(ty: FloatType) -> Type {
    DirectType(TyFloating(ty), noTypeQuals(), noAttributes())
}

pub fn simplePtr(ty: Type) -> Type {
    PtrType(Box::new(ty), noTypeQuals(), noAttributes())
}

pub fn constPtr(ty: Type) -> Type {
    PtrType(Box::new(ty), TypeQuals { constant: true, .. Default::default() }, noAttributes())
}

pub fn uint16_tType() -> Type {
    integral(TyUShort)
}

pub fn uint32_tType() -> Type {
    integral(TyUInt)
}

pub fn uint64_tType() -> Type {
    integral(TyULLong)
}

pub fn size_tType() -> Type {
    integral(TyInt)
}

pub fn ptrDiffType() -> Type {
    integral(TyInt)
}

pub fn boolType() -> Type {
    integral(TyInt)
}

pub fn voidType() -> Type {
    DirectType(TyVoid, noTypeQuals(), noAttributes())
}

pub fn voidPtr() -> Type {
    simplePtr(voidType())
}

pub fn constVoidPtr() -> Type {
    constPtr(voidType())
}

pub fn charPtr() -> Type {
    simplePtr(integral(TyChar))
}

pub fn constCharPtr() -> Type {
    constPtr(integral(TyChar))
}

pub fn stringType() -> Type {
    ArrayType(Box::new(DirectType(TyIntegral(TyChar),
                                  TypeQuals { constant: true, .. Default::default() },
                                  noAttributes())),
              UnknownArraySize(false),
              noTypeQuals(),
              noAttributes())
}

pub fn valistType() -> Type {
    DirectType(TyBuiltin(TyVaList), noTypeQuals(), noAttributes())
}

pub fn isIntegralType(ty: &Type) -> bool {
    match *ty {
        DirectType(TyIntegral(_), ..) => true,
        DirectType(TyEnum(_), ..) => true,
        _ => false,
    }
}

pub fn isFloatingType(ty: &Type) -> bool {
    match *ty {
        DirectType(TyFloating(_), ..) => true,
        _ => false,
    }
}

pub fn isPointerType(ty: &Type) -> bool {
    match *ty {
        PtrType(..) => true,
        ArrayType(..) => true,
        _ => false,
    }
}

pub fn isScalarType(ty: &Type) -> bool {
    isIntegralType(ty) || isPointerType(ty) || isFloatingType(ty)
}

pub fn isFunctionType(ty: &Type) -> bool {
    match *ty {
        TypeDefType(box TypeDefRef(_, ref actual_ty, _), ..) => isFunctionType(actual_ty),
        FunctionType(..) => true,
        _ => false,
    }
}

pub fn typeQuals(ty: Type) -> TypeQuals {
    match ty {
        DirectType(_, q, _) => q,
        PtrType(_, q, _) => q,
        ArrayType(_, _, q, _) => q,
        FunctionType(_, _) => noTypeQuals(),
        TypeDefType(box TypeDefRef(_, t, _), q, _) => mergeTypeQuals(q, typeQuals(t)),
    }
}

pub fn typeQualsUpd<F>(f: F, ty: Type) -> Type
    where F: FnOnce(TypeQuals) -> TypeQuals
{
    match ty {
        DirectType(ty_name, ty_quals, ty_attrs) => DirectType(ty_name, f(ty_quals), ty_attrs),
        PtrType(ty_inner, ty_quals, ty_attrs) => PtrType(ty_inner, f(ty_quals), ty_attrs),
        ArrayType(ty_inner, sz, ty_quals, ty_attrs) => {
            ArrayType(ty_inner, sz, f(ty_quals), ty_attrs)
        }
        FunctionType(ty_inner, ty_attrs) => FunctionType(ty_inner, ty_attrs),
        TypeDefType(ty_ref, ty_quals, ty_attrs) => TypeDefType(ty_ref, f(ty_quals), ty_attrs),
    }
}

pub fn typeAttrs(ty: Type) -> Attributes {
    match ty {
        DirectType(_, _, a) => a,
        PtrType(_, _, a) => a,
        ArrayType(_, _, _, a) => a,
        FunctionType(_, a) => a,
        TypeDefType(box TypeDefRef(_, t, _), _, a) => mergeAttributes(a, typeAttrs(t)),
    }
}

pub fn typeAttrsUpd<F>(f: F, ty: Type) -> Type
    where F: FnOnce(Attributes) -> Attributes
{
    match ty {
        DirectType(ty_name, ty_quals, ty_attrs) => DirectType(ty_name, ty_quals, f(ty_attrs)),
        PtrType(ty_inner, ty_quals, ty_attrs) => PtrType(ty_inner, ty_quals, f(ty_attrs)),
        ArrayType(ty_inner, sz, ty_quals, ty_attrs) => {
            ArrayType(ty_inner, sz, ty_quals, f(ty_attrs))
        }
        FunctionType(ty_inner, ty_attrs) => FunctionType(ty_inner, f(ty_attrs)),
        TypeDefType(ty_ref, ty_quals, ty_attrs) => TypeDefType(ty_ref, ty_quals, f(ty_attrs)),
    }
}

pub fn baseType(ty: Type) -> Type {
    match ty {
        PtrType(t, _, _) => *t,
        ArrayType(t, _, _, _) => *t,
        _ => panic!("base of non-pointer type"),
    }
}

pub fn derefTypeDef(ty: Type) -> Type {
    match ty {
        TypeDefType(box TypeDefRef(_, t, _), q, a) => {
            typeAttrsUpd(|b| mergeAttributes(a, b),
                         typeQualsUpd(|b| mergeTypeQuals(q, b), derefTypeDef(t)))
        }
        ty => ty,
    }
}

pub fn deepDerefTypeDef(ty: Type) -> Type {
    match ty {
        PtrType(t, quals, attrs) => PtrType(box deepDerefTypeDef(*t), quals, attrs),
        ArrayType(t, size, quals, attrs) => ArrayType(box deepDerefTypeDef(*t), size, quals, attrs),
        FunctionType(funtype, attrs) => {
            // XXX I can't match on *funtype directly for some reason?
            let unboxed = *funtype;
            match unboxed {
                FunType(rt, params, varargs) =>
                    FunctionType(box FunType(deepDerefTypeDef(rt), params, varargs), attrs),
                FunTypeIncomplete(rt) =>
                    FunctionType(box FunTypeIncomplete(deepDerefTypeDef(rt)), attrs)
            }
        }
        TypeDefType(box TypeDefRef(_, t, _), q, a) => {
            typeAttrsUpd(|b| mergeAttributes(a, b),
                         typeQualsUpd(|b| mergeTypeQuals(q, b), deepDerefTypeDef(t)))
        }
        t => t,
    }
}

pub fn isVariablyModifiedType(ty: Type) -> bool {

    pub fn isVariableArraySize(sz: &ArraySize) -> bool {
        match *sz {
            UnknownArraySize(isStarred) => isStarred,
            ArraySize(isStatic, ref e) => isStatic || isConstantSize(e),
        }
    }

    pub fn isConstantSize(expr: &Expr) -> bool {
        match *expr {
            CExpression::CConst(CConstant::CIntConst { .. }) => true,
            _ => false,
        }
    }

    match derefTypeDef(ty) {
        TypeDefType { .. } => panic!("impossible: derefTypeDef returned a TypeDefType"),
        DirectType { .. } => false,
        PtrType(ptr_ty, _, _) => isVariablyModifiedType(*ptr_ty),
        ArrayType(_, sz, _, _) => isVariableArraySize(&sz),
        FunctionType { .. } => false,
    }
}

pub fn sameType(t1: Type, t2: Type) -> bool {

    let sameType_q = match (derefTypeDef(t1.clone()), derefTypeDef(t2.clone())) {
        (TypeDefType { .. }, _) => {
            panic!("impossible: derefTypeDef t1 returned a TypeDefType")
        }
        (_, TypeDefType { .. }) => {
            panic!("impossible: derefTypeDef t2 returned a TypeDefType")
        }
        (DirectType(tn1, q1, _a1), DirectType(tn2, q2, _a2)) => {
            sameTypeName(tn1, tn2) && sameQuals(q1, q2)
        }
        (PtrType(pt1, q1, _a1), PtrType(pt2, q2, _a2)) => sameType(*pt1, *pt2) && sameQuals(q1, q2),
        (ArrayType(at1, sz1, q1, _a1), ArrayType(at2, sz2, q2, _a2)) => {
            sameType(*at1, *at2) && sameArraySize(sz1, sz2) && sameQuals(q1, q2)
        }
        (FunctionType(ft1, _a1), FunctionType(ft2, _a2)) => sameFunType(*ft1, *ft2),
        _ => false,
    };

    !(isVariablyModifiedType(t1) || isVariablyModifiedType(t2)) && sameType_q
}

pub fn sameTypeName(t1: TypeName, t2: TypeName) -> bool {
    match (t1, t2) {
        (TyVoid, TyVoid) => true,
        (TyIntegral(i1), TyIntegral(i2)) => i1 == i2,
        (TyFloating(f1), TyFloating(f2)) => f1 == f2,
        (TyComplex(f1), TyComplex(f2)) => f1 == f2,
        (TyComp(ctr1), TyComp(ctr2)) => sameCompTypeRef(ctr1, ctr2),
        (TyEnum(etr1), TyEnum(etr2)) => sameEnumTypeRef(etr1, etr2),
        (TyBuiltin(b1), TyBuiltin(b2)) => sameBuiltinType(b1, b2),
        _ => false,
    }
}

pub fn sameBuiltinType(_0: BuiltinType, _1: BuiltinType) -> bool {
    match (_0, _1) {
        (TyVaList, TyVaList) => true,
        (TyAny, TyAny) => false,
        (_, _) => false,
    }
}

pub fn sameCompTypeRef(CompTypeRef(sue1, kind1, _): CompTypeRef,
                       CompTypeRef(sue2, kind2, _): CompTypeRef)
                       -> bool {
    sue1 == sue2 && kind1 == kind2
}

pub fn sameEnumTypeRef(EnumTypeRef(sue1, _): EnumTypeRef,
                       EnumTypeRef(sue2, _): EnumTypeRef)
                       -> bool {
    sue1 == sue2
}

pub fn sameFunType(_0: FunType, _1: FunType) -> bool {
    fn sameParamDecls(pl1: &[ParamDecl], pl2: &[ParamDecl]) -> bool {
        pl1.len() == pl2.len() &&
            pl1.iter().zip(pl2).all(|(x, y)| sameType(declType(x), declType(y)))
    }

    match (_0, _1) {
        (FunType(rt1, params1, isVar1), FunType(rt2, params2, isVar2)) => {
            sameType(rt1, rt2) && sameParamDecls(&params1, &params2) && isVar1 == isVar2
        }
        (FunTypeIncomplete(rt1), FunTypeIncomplete(rt2)) => sameType(rt1, rt2),
        (_, _) => false,
    }
}

pub fn sameArraySize(_0: ArraySize, _1: ArraySize) -> bool {
    fn sizeEqual(a: Expr, b: Expr) -> bool {
        match (a, b) {
            (CExpression::CConst(CConstant::CIntConst(i1, _)),
             CExpression::CConst(CConstant::CIntConst(i2, _))) => i1 == i2,
            (oe1, oe2) => oe1.node_info() == oe2.node_info()
        }
    }

    match (_0, _1) {
        (UnknownArraySize(isStar1), UnknownArraySize(isStar2)) => isStar1 == isStar2,
        (ArraySize(s1, e1), ArraySize(s2, e2)) => s1 == s2 && sizeEqual(e1, e2),
        (_, _) => false,
    }
}

pub fn sameQuals(TypeQuals { constant: c1, volatile: v1, restrict: r1, .. }: TypeQuals,
                 TypeQuals { constant: c2, volatile: v2, restrict: r2, .. }: TypeQuals) -> bool {
    c1 == c2 && v1 == v2 && r1 == r2
}

pub fn canonicalType(ty: Type) -> Type {
    match deepDerefTypeDef(ty) {
        FunctionType(ft, attrs) => simplePtr(FunctionType(ft, attrs)),
        ty => ty,
    }
}
