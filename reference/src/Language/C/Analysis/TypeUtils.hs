module Language.C.Analysis.TypeUtils (
    -- * Constructors
    integral,
    floating,
    simplePtr,
    uint16_tType,
    uint32_tType,
    uint64_tType,
    size_tType,
    ptrDiffType,
    boolType,
    voidType,
    voidPtr,
    constVoidPtr,
    charPtr,
    constCharPtr,
    stringType,
    valistType,
    -- * Classifiers
    isIntegralType,
    isFloatingType,
    isPointerType,
    isScalarType,
    isFunctionType,
    -- Extractors
    typeQuals,
    typeQualsUpd,
    typeAttrs,
    typeAttrsUpd,
    baseType,
    derefTypeDef,
    deepDerefTypeDef,
    canonicalType,
    -- * Type comparisons
    sameType,
    -- * Other utilities
    getIntType,
    getFloatType
) where

import Language.C.Analysis.SemRep
import Language.C.Data.Node (CNode(..))
import Language.C.Syntax.AST (CExpression (..), CConstant (..))
import Language.C.Syntax.Constants

-- | Constructor for a simple integral type.
integral :: IntType -> Type
integral ty = DirectType (TyIntegral ty) noTypeQuals noAttributes

-- | Constructor for a simple floating-point type.
floating :: FloatType -> Type
floating ty = DirectType (TyFloating ty) noTypeQuals noAttributes

-- | A simple pointer with no qualifiers
simplePtr :: Type -> Type
simplePtr t = PtrType t noTypeQuals []

-- | A pointer with the @const@ qualifier.
constPtr :: Type -> Type
constPtr t = PtrType t (noTypeQuals { constant = True }) []

-- | The underlying type for @uint16_t@. For now, this is just @unsigned short@.
uint16_tType :: Type
uint16_tType = integral TyUShort

-- | The underlying type for @uint32_t@. For now, this is just @unsigned int@.
uint32_tType :: Type
uint32_tType = integral TyUInt

-- | The underlying type for @uint64_t@. For now, this is just @unsigned long long@.
uint64_tType :: Type
uint64_tType = integral TyULLong

-- | The type returned by sizeof (size_t). For now, this is just @int@.
size_tType :: Type
size_tType = integral TyInt

-- | The type of pointer differences (ptrdiff_t). For now, this is just @int@.
ptrDiffType :: Type
ptrDiffType = integral TyInt

-- | The type of comparisons\/guards. This is always just @int@.
boolType :: Type
boolType = integral TyInt

-- | Simple @void@ type.
voidType :: Type
voidType = DirectType TyVoid noTypeQuals noAttributes

-- | An unqualified @void@ pointer.
voidPtr :: Type
voidPtr = simplePtr voidType

-- | A @const@-qualified @void@ pointer.
constVoidPtr :: Type
constVoidPtr = constPtr voidType

-- | An unqualified @char@ pointer.
charPtr :: Type
charPtr = simplePtr (integral TyChar)

-- | A @const@-qualified @char@ pointer.
constCharPtr :: Type
constCharPtr = constPtr (integral TyChar)

-- | The type of a constant string.
stringType :: Type
stringType  = ArrayType
              (DirectType (TyIntegral TyChar)
                          (noTypeQuals { constant = True })
                          noAttributes)
              (UnknownArraySize False)
              noTypeQuals
              []

-- | The builtin type of variable-length argument lists.
valistType :: Type
valistType  = DirectType (TyBuiltin TyVaList) noTypeQuals noAttributes

-- | Check whether a type is an integral type. This includes @enum@
--   types. This function does not attempt to resolve @typedef@ types.
isIntegralType :: Type -> Bool
isIntegralType (DirectType (TyIntegral _) _ _) = True
isIntegralType (DirectType (TyEnum _) _ _)     = True
isIntegralType _                               = False

-- | Check whether a type is a floating-point numeric type. This
--   function does not attempt to resolve @typedef@ types.
isFloatingType :: Type -> Bool
isFloatingType (DirectType (TyFloating _) _ _) = True
isFloatingType _                               = False

-- | Check whether a type is an pointer type. This includes array
--   types. This function does not attempt to resolve @typedef@ types.
isPointerType :: Type -> Bool
isPointerType (PtrType _ _ _)     = True
isPointerType (ArrayType _ _ _ _) = True
isPointerType _                   = False

-- | Check whether a type is a scalar type. Scalar types include
--   arithmetic types and pointer types.
isScalarType :: Type -> Bool
isScalarType t = isIntegralType t || isPointerType t || isFloatingType t

-- | return @True@ if the given type is a function type
--
--   Result is undefined in the presence of undefined typeDefs
isFunctionType :: Type -> Bool
isFunctionType ty =
    case ty of  TypeDefType (TypeDefRef _ actual_ty _) _ _ -> isFunctionType actual_ty
                FunctionType _ _ -> True
                _ -> False

-- | Return the qualifiers of a type.
typeQuals :: Type -> TypeQuals
typeQuals (DirectType _ q _) = q
typeQuals (PtrType _ q _) = q
typeQuals (ArrayType _ _ q _) = q
typeQuals (FunctionType _ _) = noTypeQuals
typeQuals (TypeDefType (TypeDefRef _ t _) q _) = mergeTypeQuals q (typeQuals t)

--  |Update type qualifiers
--   For function types, it is an error to change any type qualifiers
--   For typedef types, the result is stored in the typedef attribute field
typeQualsUpd :: (TypeQuals -> TypeQuals) -> Type -> Type
typeQualsUpd f ty =
    case ty of DirectType ty_name ty_quals ty_attrs -> DirectType ty_name (f ty_quals) ty_attrs
               PtrType ty_inner ty_quals ty_attrs         -> PtrType ty_inner (f ty_quals) ty_attrs
               ArrayType ty_inner sz ty_quals ty_attrs    -> ArrayType ty_inner sz (f ty_quals) ty_attrs
               FunctionType ty_inner ty_attrs             -> FunctionType ty_inner ty_attrs
               TypeDefType ty_ref ty_quals ty_attrs -> TypeDefType ty_ref (f ty_quals) ty_attrs

-- | Return the attributes of a type.
typeAttrs :: Type -> Attributes
typeAttrs (DirectType _ _ a) = a
typeAttrs (PtrType _ _ a) = a
typeAttrs (ArrayType _ _ _ a) = a
typeAttrs (FunctionType _ a) = a
typeAttrs (TypeDefType (TypeDefRef _ t _) _ a) = mergeAttributes a (typeAttrs t)

--  |Update type attributes
typeAttrsUpd :: (Attributes -> Attributes) -> Type -> Type
typeAttrsUpd f ty =
    case ty of DirectType ty_name ty_quals ty_attrs -> DirectType ty_name ty_quals (f ty_attrs)
               PtrType ty_inner ty_quals ty_attrs         -> PtrType ty_inner ty_quals (f ty_attrs)
               ArrayType ty_inner sz ty_quals ty_attrs    -> ArrayType ty_inner sz ty_quals (f ty_attrs)
               FunctionType ty_inner ty_attrs             -> FunctionType ty_inner (f ty_attrs)
               TypeDefType ty_ref ty_quals ty_attrs -> TypeDefType ty_ref ty_quals (f ty_attrs)

-- | Return the base type of a pointer or array type. It is an error
--   to call this function with a type that is not in one of those two
--   categories.
baseType :: Type -> Type
baseType (PtrType t _ _)     = t
baseType (ArrayType t _ _ _) = t
baseType _                   = error "base of non-pointer type"

-- | resolve typedefs, if possible
derefTypeDef :: Type -> Type
derefTypeDef (TypeDefType (TypeDefRef _ t _) q a) =
  (typeAttrsUpd (mergeAttributes a) . typeQualsUpd (mergeTypeQuals q))
  (derefTypeDef t)
derefTypeDef ty = ty

-- | Attempt to remove all references to @typedef@ types from a given type.
--   Note that this does not dereference the types of structure or union
--   fields, so there are still cases where further dereferencing is
--   needed.
deepDerefTypeDef :: Type -> Type
deepDerefTypeDef (PtrType t quals attrs) =
  PtrType (deepDerefTypeDef t) quals attrs
deepDerefTypeDef (ArrayType t size quals attrs) =
  ArrayType (deepDerefTypeDef t) size quals attrs
deepDerefTypeDef (FunctionType (FunType rt params varargs) attrs) =
  FunctionType (FunType (deepDerefTypeDef rt) params varargs) attrs
deepDerefTypeDef (FunctionType (FunTypeIncomplete rt) attrs) =
  FunctionType (FunTypeIncomplete (deepDerefTypeDef rt)) attrs
deepDerefTypeDef (TypeDefType (TypeDefRef _ t _) q a) =
  (typeAttrsUpd (mergeAttributes a) . typeQualsUpd (mergeTypeQuals q))
  (deepDerefTypeDef t)
deepDerefTypeDef t = t

-- | True iff Type is a variable length array or a derived type thereof.
-- Variably modified types have function or block scope, so only some
-- constructions are possible.
isVariablyModifiedType :: Type -> Bool
isVariablyModifiedType t =
  case derefTypeDef t of
    TypeDefType {} -> error "impossible: derefTypeDef t returned a TypeDefType"
    DirectType {} -> False
    PtrType ptr_ty _ _ -> isVariablyModifiedType ptr_ty
    ArrayType _ sz _ _ -> isVariableArraySize sz
    FunctionType {} -> False
  where
    isVariableArraySize :: ArraySize -> Bool
    isVariableArraySize (UnknownArraySize isStarred) = isStarred
    isVariableArraySize (ArraySize isStatic e) = isStatic || isConstantSize e

    isConstantSize :: Expr -> Bool
    isConstantSize (CConst (CIntConst {})) = True
    isConstantSize _ = False

-- | Two types denote the same type if they are identical, ignoring type
-- definitions, and neither is a variably modified type.
sameType :: Type -> Type -> Bool
sameType t1 t2 =
  not (isVariablyModifiedType t1 || isVariablyModifiedType t2) && sameType'
  where
    sameType' =
      case (derefTypeDef t1, derefTypeDef t2) of
        (TypeDefType {}, _) -> error "impossible: derefTypeDef t1 returned a TypeDefType"
        (_, TypeDefType {}) -> error "impossible: derefTypeDef t2 returned a TypeDefType"
        (DirectType tn1 q1 _a1, DirectType tn2 q2 _a2) ->
          sameTypeName tn1 tn2 && sameQuals q1 q2 {- FIXME: same attributes? -}
        (PtrType pt1 q1 _a1, PtrType pt2 q2 _a2) ->
          sameType pt1 pt2 && sameQuals q1 q2
        (ArrayType at1 sz1 q1 _a1, ArrayType at2 sz2 q2 _a2) ->
          sameType at1 at2 && sameArraySize sz1 sz2 && sameQuals q1 q2
        (FunctionType ft1 _a1, FunctionType ft2 _a2) ->
          sameFunType ft1 ft2
        _ -> False

sameTypeName :: TypeName -> TypeName -> Bool
sameTypeName t1 t2 =
  case (t1, t2) of
    (TyVoid, TyVoid) -> True
    (TyIntegral i1, TyIntegral i2) -> i1 == i2
    (TyFloating f1, TyFloating f2) -> f1 == f2
    (TyComplex f1, TyComplex f2) -> f1 == f2
    (TyComp ctr1, TyComp ctr2) -> sameCompTypeRef ctr1 ctr2
    (TyEnum etr1, TyEnum etr2) -> sameEnumTypeRef etr1 etr2
    (TyBuiltin b1, TyBuiltin b2) -> sameBuiltinType b1 b2
    _ -> False

sameBuiltinType :: BuiltinType -> BuiltinType -> Bool
sameBuiltinType TyVaList TyVaList = True
sameBuiltinType TyAny TyAny = False {- what does TyAny mean? -}
sameBuiltinType _ _ = False

sameCompTypeRef :: CompTypeRef -> CompTypeRef -> Bool
sameCompTypeRef (CompTypeRef sue1 kind1 _) (CompTypeRef sue2 kind2 _) =
  sue1 == sue2 && kind1 == kind2

sameEnumTypeRef :: EnumTypeRef -> EnumTypeRef -> Bool
sameEnumTypeRef (EnumTypeRef sue1 _) (EnumTypeRef sue2 _) = sue1 == sue2

sameFunType :: FunType -> FunType -> Bool
sameFunType (FunType rt1 params1 isVar1) (FunType rt2 params2 isVar2) =
  sameType rt1 rt2 && sameParamDecls params1 params2 && isVar1 == isVar2
  where
    sameParamDecls :: [ParamDecl] -> [ParamDecl] -> Bool
    sameParamDecls param_list1 param_list2 =
      length param_list1 == length param_list2
      && and (zipWith sameParamDecl param_list1 param_list2)
    -- ignores param identifiers, just compares types
    sameParamDecl :: ParamDecl -> ParamDecl -> Bool
    sameParamDecl p1 p2 = sameType (declType p1) (declType p2)
sameFunType (FunTypeIncomplete rt1) (FunTypeIncomplete rt2) =
  sameType rt1 rt2
sameFunType _ _ = False

-- | Returns 'True' iff both array sizes denote the same size.  Assumes that
-- neither array type was a variably modified type.
sameArraySize :: ArraySize -> ArraySize -> Bool
sameArraySize (UnknownArraySize isStar1) (UnknownArraySize isStar2) = isStar1 == isStar2
sameArraySize (ArraySize s1 e1) (ArraySize s2 e2) = s1 == s2 && sizeEqual e1 e2
  where
    -- FIXME: Do something better, and combine with sizeEqual in Language.C.Analysis.TypeCheck
    sizeEqual :: Expr -> Expr -> Bool
    sizeEqual (CConst (CIntConst i1 _)) (CConst (CIntConst i2 _)) = i1 == i2
    sizeEqual oe1 oe2 = nodeInfo oe1 == nodeInfo oe2
sameArraySize _ _ = False

sameQuals :: TypeQuals -> TypeQuals -> Bool
sameQuals (TypeQuals {constant = c1, volatile = v1, restrict = r1})
          (TypeQuals {constant = c2, volatile = v2, restrict = r2}) =
  c1 == c2 && v1 == v2 && r1 == r2

canonicalType :: Type -> Type
canonicalType t =
  case deepDerefTypeDef t of
    FunctionType ft attrs -> simplePtr (FunctionType ft attrs)
    t'                    -> t'

-- XXX: move to be with other flag functions
testFlags :: Enum f => [f] -> Flags f -> Bool
testFlags flags fi = all (`testFlag` fi) flags

-- XXX: deal with FlagImag. No representation for it in Complex.
-- XXX: deal with invalid combinations of flags?
getIntType :: Flags CIntFlag -> IntType
getIntType flags | testFlags [FlagLongLong, FlagUnsigned] flags = TyULLong
                 | testFlag  FlagLongLong flags                 = TyLLong
                 | testFlags [FlagLong, FlagUnsigned] flags     = TyULong
                 | testFlag  FlagLong flags                     = TyLong
                 | testFlag  FlagUnsigned flags                 = TyUInt
                 | otherwise                                    = TyInt

getFloatType :: String -> FloatType
getFloatType fs | last fs `elem` ['f', 'F'] = TyFloat
                | last fs `elem` ['l', 'L'] = TyLDouble
                | otherwise                 = TyDouble

