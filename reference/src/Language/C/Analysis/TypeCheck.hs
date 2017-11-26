{-# LANGUAGE FlexibleInstances, CPP #-}
module Language.C.Analysis.TypeCheck where

import Control.Monad
import Data.Maybe
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops
import Language.C.Analysis.DefTable
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Analysis.TypeConversions
import Language.C.Analysis.TypeUtils
import Language.C.Analysis.Debug ()
import Text.PrettyPrint.HughesPJ

-- We used to re-implement and export the standard Either instance for
-- Monad, which is bad, because as of GHC 7 it is in Control.Monad.Instances
-- in base >4.2. For backwards compatibility with ghc-6.X, we use CPP here.
#if __GLASGOW_HASKELL__ < 700
instance Monad (Either String) where
    return        = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r
    fail msg      = Left msg
#endif

pType :: Type -> String
pType = render . pretty

typeErrorOnLeft :: (MonadCError m) => NodeInfo -> Either String a -> m a
typeErrorOnLeft ni (Left err) = typeError ni err
typeErrorOnLeft _  (Right v)  = return v

-- XXX: this should use a custom error type, but typeMismatch isn't always right
typeError :: MonadCError m => NodeInfo -> String -> m a
typeError = astError

notFound :: Ident -> Either String a
notFound i = Left $ "not found: " ++ identToString i

checkScalar' :: MonadCError m => NodeInfo -> Type -> m ()
checkScalar' ni = typeErrorOnLeft ni . checkScalar

checkIntegral' :: MonadCError m => NodeInfo -> Type -> m ()
checkIntegral' ni = typeErrorOnLeft ni . checkIntegral

assignCompatible' :: MonadCError m =>
                     NodeInfo -> CAssignOp -> Type -> Type -> m ()
assignCompatible' ni op t1 t2 = typeErrorOnLeft ni (assignCompatible op t1 t2)

binopType' :: MonadCError m =>
              NodeInfo -> CBinaryOp -> Type -> Type -> m Type
binopType' ni op t1 t2 = typeErrorOnLeft ni (binopType op t1 t2)

conditionalType' :: MonadCError m => NodeInfo -> Type -> Type -> m Type
conditionalType' ni t1 t2 = typeErrorOnLeft ni $ conditionalType t1 t2

checkScalar :: Type -> Either String ()
checkScalar t =
  case canonicalType t of
    DirectType _ _ _  -> Right ()
    PtrType _ _ _     -> Right ()
    ArrayType _ _ _ _ -> Right () -- because it's just a pointer
    t' -> Left $
          "expected scalar type, got: "
          ++ pType t ++ " (" ++ pType t' ++ ")"

checkIntegral :: Type -> Either String ()
checkIntegral t | isIntegralType (canonicalType t) = Right ()
                | otherwise = Left $
                              "expected integral type, got: " ++
                              pType t ++ " (" ++
                              pType (canonicalType t) ++ ")"

-- | Determine the type of a constant.
constType :: (MonadCError m, MonadName m) => CConst -> m Type
constType (CIntConst (CInteger _ _ flags) _) =
  return $ DirectType (TyIntegral (getIntType flags)) noTypeQuals noAttributes
constType (CCharConst (CChar _ True) _) =
  return $ DirectType (TyIntegral TyInt) noTypeQuals noAttributes
constType (CCharConst (CChar _ False) _) =
  return $ DirectType (TyIntegral TyChar) noTypeQuals noAttributes
constType (CCharConst (CChars _ _) _)  =
  return $ DirectType (TyIntegral TyInt) noTypeQuals noAttributes -- XXX
constType (CFloatConst (CFloat fs) _) =
  return $ DirectType (TyFloating (getFloatType fs)) noTypeQuals noAttributes
-- XXX: should strings have any type qualifiers or attributes?
constType (CStrConst (CString chars wide) ni) =
  do n <- genName
     let charType | wide      = TyInt -- XXX: this isn't universal
                  | otherwise = TyChar
         ni' = mkNodeInfo (posOf ni) n
         arraySize = ArraySize
                     True -- XXX: is it static?
                     (CConst
                      (CIntConst
                       (cInteger (toInteger (length chars))) ni'))
     return $ ArrayType (DirectType (TyIntegral charType) noTypeQuals noAttributes)
                        arraySize noTypeQuals []

-- | Determine whether two types are compatible.
compatible :: Type -> Type -> Either String ()
compatible t1 t2 = void$ compositeType t1 t2

-- | Determine the composite type of two compatible types.
compositeType :: Type -> Type -> Either String Type
compositeType t1 (DirectType (TyBuiltin TyAny) _ _) = Right t1
compositeType (DirectType (TyBuiltin TyAny) _ _) t2 = Right t2
compositeType t1@(DirectType tn1 q1 a1) t2@(DirectType tn2 q2 a2) =
  do tn <- case (tn1, tn2) of
             (TyVoid, TyVoid) -> Right TyVoid
             (TyIntegral _, TyEnum _) -> Right tn1
             (TyEnum _, TyIntegral _) -> Right tn2
             (TyIntegral i1, TyIntegral i2) ->
               Right $ TyIntegral (intConversion i1 i2)
             (TyFloating f1, TyFloating f2) ->
               Right $ TyFloating (floatConversion f1 f2)
             (TyComplex f1, TyComplex f2) ->
               Right $ TyComplex (floatConversion f1 f2)
             (TyComp c1, TyComp c2) ->
               do when (sueRef c1 /= sueRef c2) $
                       Left $ "incompatible composite types: "
                              ++ pType t1 ++ ", " ++ pType t2
                  Right tn1
             (TyEnum e1, TyEnum e2) ->
               do when (sueRef e1 /= sueRef e2) $
                       Left $ "incompatible enumeration types: "
                              ++ pType t1 ++ ", " ++ pType t2
                  Right $ TyEnum e1
             (TyBuiltin TyVaList, TyBuiltin TyVaList) ->
               Right $ TyBuiltin TyVaList
             (TyBuiltin _, TyBuiltin _) ->
               Left $ "incompatible builtin types: "
                      ++ pType t1 ++ ", " ++ pType t2
             (_, _) -> Left $ "incompatible direct types: "
                       ++ pType t1 ++ ", " ++ pType t2
     Right $ DirectType tn (mergeTypeQuals q1 q2) (mergeAttributes a1 a2)
compositeType (PtrType t1 q1 a1) (PtrType (DirectType TyVoid _ _) q2 _) =
  Right $ PtrType t1 (mergeTypeQuals q1 q2) a1
compositeType (PtrType (DirectType TyVoid _ _) q1 _) (PtrType t2 q2 a2) =
  Right $ PtrType t2 (mergeTypeQuals q1 q2) a2
compositeType (PtrType t1 q1 a1) t2 | isIntegralType t2 =
  Right $ PtrType t1 (mergeTypeQuals q1 (typeQuals t2)) a1
compositeType t1 (PtrType t2 q2 a2) | isIntegralType t1 =
  Right $ PtrType t2 (mergeTypeQuals (typeQuals t1) q2) a2
compositeType (ArrayType t1 _sz1 q1 a1) t2 | isIntegralType t2 =
  Right $ PtrType t1 q1 a1
compositeType t1 (ArrayType t2 _sz2 q2 a2) | isIntegralType t1 =
  Right $ PtrType t2 q2 a2
compositeType (ArrayType t1 s1 q1 a1) (ArrayType t2 s2 q2 a2) =
  do t <- compositeType t1 t2
     s <- compositeSize s1 s2
     let quals = mergeTypeQuals q1 q2
         attrs = mergeAttrs a1 a2
     Right (ArrayType t s quals attrs)
compositeType t1 t2 | isPointerType t1 && isPointerType t2 =
  do t <- compositeType (baseType t1) (baseType t2)
     let quals = mergeTypeQuals (typeQuals t1) (typeQuals t2)
         attrs = mergeAttrs (typeAttrs t1) (typeAttrs t2)
     Right (PtrType t quals attrs)
compositeType (TypeDefType tdr1 _q1 _a1) (TypeDefType tdr2 _q2 _a2) =
  case (tdr1, tdr2) of
    (TypeDefRef _ t1 _, TypeDefRef _ t2 _) ->
      compositeType t1 t2
compositeType (FunctionType ft1 attrs1) (FunctionType ft2 attrs2) =
  case (ft1, ft2) of
    (FunType rt1 args1 varargs1, FunType rt2 args2 varargs2) ->
      do {- when (length args1 /= length args2) $
              Left "different numbers of arguments in function types" -}
         args <- zipWithM compositeParamDecl args1 args2
         when (varargs1 /= varargs2) $
              Left "incompatible varargs declarations"
         doFunType rt1 rt2 args varargs1
    (FunType rt1 args1 varargs1, FunTypeIncomplete rt2) ->
      doFunType rt1 rt2 args1 varargs1
    (FunTypeIncomplete rt1, FunType rt2 args2 varargs2) ->
      doFunType rt1 rt2 args2 varargs2
    (FunTypeIncomplete rt1, FunTypeIncomplete rt2) ->
      do rt <- compositeType rt1 rt2
         Right (FunctionType (FunTypeIncomplete rt) (mergeAttrs attrs1 attrs2))
  where doFunType rt1 rt2 args varargs =
          do rt <- compositeType rt1 rt2
             Right (FunctionType
                     (FunType rt args varargs)
                     (mergeAttrs attrs1 attrs2))
compositeType t1 t2 = Left $ "incompatible types: "
                         ++ pType t1 ++ ", " ++ pType t2

-- XXX: this may not be correct
compositeSize :: ArraySize -> ArraySize -> Either String ArraySize
compositeSize (UnknownArraySize _) s2 = Right s2
compositeSize s1 (UnknownArraySize _) = Right s1
compositeSize (ArraySize s1 e1) (ArraySize s2 e2)
  | s1 == s2 && sizeEqual e1 e2 = Right $ ArraySize s1 e1
  | otherwise = Right $ ArraySize s1 e1
{-
    fail $ "incompatible array sizes: "
           ++ (render . pretty) e1 ++ ", " ++ (render . pretty) e2
-}

sizeEqual :: CExpr -> CExpr -> Bool
sizeEqual (CConst (CIntConst i1 _)) (CConst (CIntConst i2 _)) = i1 == i2
sizeEqual e1 e2 = nodeInfo e1 == nodeInfo e2

mergeAttrs :: Attributes -> Attributes -> Attributes
mergeAttrs = (++) -- XXX: ultimately this should be smarter

compositeParamDecl :: ParamDecl -> ParamDecl -> Either String ParamDecl
compositeParamDecl (ParamDecl vd1 ni1) (ParamDecl vd2 _) =
  compositeParamDecl' ParamDecl vd1 vd2 ni1
compositeParamDecl (AbstractParamDecl vd1 _) (ParamDecl vd2 ni2) =
  compositeParamDecl' ParamDecl vd1 vd2 ni2
compositeParamDecl (ParamDecl vd1 ni1) (AbstractParamDecl vd2 _) =
  compositeParamDecl' ParamDecl vd1 vd2 ni1
compositeParamDecl (AbstractParamDecl vd1 ni1) (AbstractParamDecl vd2 _) =
  compositeParamDecl' AbstractParamDecl vd1 vd2 ni1

compositeParamDecl' :: (VarDecl -> NodeInfo -> ParamDecl)
                    -> VarDecl
                    -> VarDecl
                    -> NodeInfo
                    -> Either String ParamDecl
compositeParamDecl' f (VarDecl n1 attrs1 t1) (VarDecl n2 attrs2 t2) dni =
  do vd <- compositeVarDecl (VarDecl n1 attrs1 t1') (VarDecl n2 attrs2 t2')
     Right $ f vd dni
  where t1' = canonicalType t1
        t2' = canonicalType t2

compositeVarDecl :: VarDecl -> VarDecl -> Either String VarDecl
compositeVarDecl (VarDecl n1 attrs1 t1) (VarDecl _ attrs2 t2) =
  do t <- compositeType t1 t2
     Right (VarDecl n1 (compositeDeclAttrs attrs1 attrs2) t)

-- XXX: bad treatement of inline and storage
compositeDeclAttrs :: DeclAttrs -> DeclAttrs -> DeclAttrs
compositeDeclAttrs (DeclAttrs inl stor attrs1) (DeclAttrs _ _ attrs2) =
  DeclAttrs inl stor (mergeAttrs attrs1 attrs2)

castCompatible :: Type -> Type -> Either String ()
castCompatible t1 t2 =
  case (canonicalType t1, canonicalType t2) of
    (DirectType TyVoid _ _, _) -> Right ()
    (_, _) -> checkScalar t1 >> checkScalar t2

-- | Determine whether two types are compatible in an assignment expression.
assignCompatible :: CAssignOp -> Type -> Type -> Either String ()
assignCompatible CAssignOp t1 t2 =
  case (canonicalType t1, canonicalType t2) of
    (DirectType (TyBuiltin TyAny) _ _, _) -> Right ()
    (_, DirectType (TyBuiltin TyAny) _ _) -> Right ()
    -- XXX: check qualifiers
    (PtrType (DirectType TyVoid _ _) _ _, t2') | isPointerType t2' -> Right ()
    -- XXX: check qualifiers
    (t1', PtrType (DirectType TyVoid _ _) _ _) | isPointerType t1' -> Right ()
    (PtrType _ _ _, t2') | isIntegralType t2' -> Right ()
    (t1', t2') | isPointerType t1' && isPointerType t2' ->
                 compatible (baseType t1') (baseType t2')
                --unless (typeQuals t2 <= typeQuals t1) $
                --       Left $
                --       "incompatible qualifiers in pointer assignment: "
                --       ++ pType t1 ++ ", " ++ pType t2
    (DirectType (TyComp c1) _ _, DirectType (TyComp c2) _ _)
      | sueRef c1 == sueRef c2 -> Right ()
      | otherwise -> Left $
                     "incompatible compound types in assignment: "
                     ++ pType t1 ++ ", " ++ pType t2
    (DirectType (TyBuiltin TyVaList) _ _, DirectType (TyBuiltin TyVaList) _ _) ->
      Right ()
    (DirectType tn1 _ _, DirectType tn2 _ _)
      | isJust (arithmeticConversion tn1 tn2) -> Right ()
      | otherwise -> Left $ "incompatible direct types in assignment: "
                     ++ pType t1 ++ ", " ++ pType t2
    (t1', t2') -> compatible t1' t2'
assignCompatible op t1 t2 = void$ binopType (assignBinop op) t1 t2

-- | Determine the type of a binary operation.
binopType :: CBinaryOp -> Type -> Type -> Either String Type
binopType op t1 t2 =
  case (op, canonicalType t1, canonicalType t2) of
    (_, t1', t2')
      | isLogicOp op ->
        checkScalar t1' >> checkScalar t2' >> Right boolType
      | isCmpOp op ->
        case (t1', t2') of
          (DirectType tn1 _ _, DirectType tn2 _ _) ->
                case arithmeticConversion tn1 tn2 of
                  Just _ -> Right boolType
                  Nothing -> Left $ render $
                             text "incompatible arithmetic types in comparison: "
                             <+> pretty t1 <+> text "and" <+> pretty t2
          (PtrType (DirectType TyVoid _ _) _ _, _)
            | isPointerType t2' -> Right boolType
          (_, PtrType (DirectType TyVoid _ _) _ _)
            | isPointerType t1' -> Right boolType
          (_, _)
            | isPointerType t1' && isIntegralType t2' -> Right boolType
            | isIntegralType t1' && isPointerType t2' -> Right boolType
            | isPointerType t1' && isPointerType t2' ->
              compatible t1' t2' >> Right boolType
          (_, _) -> Left "incompatible types in comparison"
    (CSubOp, ArrayType t1' _ _ _, ArrayType t2' _ _ _) ->
      compatible t1' t2' >> Right ptrDiffType
    (CSubOp, ArrayType t1' _ _ _, PtrType t2' _ _) ->
      compatible t1' t2' >> Right ptrDiffType
    (CSubOp, PtrType t1' _ _, ArrayType t2' _ _ _) ->
      compatible t1' t2' >> Right ptrDiffType
    (CSubOp, PtrType t1' _ _, PtrType t2' _ _) ->
      compatible t1' t2' >> Right ptrDiffType
    (_, PtrType _ _ _, t2')
      | isPtrOp op && isIntegralType t2' -> Right t1
      | otherwise -> Left $ "invalid pointer operation: " ++ render (pretty op)
    (CAddOp, t1', PtrType _ _ _) | isIntegralType t1' -> Right t2
    (_, ArrayType _ _ _ _, t2')
      | isPtrOp op && isIntegralType t2' -> Right t1
      | otherwise -> Left $ "invalid pointer operation: " ++ render (pretty op)
    (CAddOp, t1', ArrayType _ _ _ _) | isIntegralType t1' -> Right t2
    (_, DirectType tn1 q1 a1, DirectType tn2 q2 a2) ->
        do when (isBitOp op) (checkIntegral t1 >> checkIntegral t2)
           case arithmeticConversion tn1 tn2 of
             Just tn -> Right $ DirectType tn (mergeTypeQuals q1 q2) (mergeAttributes a1 a2)
             Nothing -> Left $ render $
                        text "invalid binary operation:" <+> pretty t1 <+> pretty op <+> pretty t2
    (_, _, _) -> Left $ render $
                 text "unhandled binary operation:" <+> pretty t1 <+> pretty op <+> pretty t2

-- | Determine the type of a conditional expression.
conditionalType :: Type -> Type -> Either String Type
conditionalType t1 t2 =
  case (canonicalType t1, canonicalType t2) of
    (PtrType (DirectType TyVoid _ _) _ _, t2') | isPointerType t2' -> Right t2
    (t1', PtrType (DirectType TyVoid _ _) _ _) | isPointerType t1' -> Right t1
    (ArrayType t1' _ q1 a1, ArrayType t2' _ q2 a2) ->
      do t <- compositeType t1' t2'
         Right $ ArrayType t (UnknownArraySize False)
                  (mergeTypeQuals q1 q2) (mergeAttrs a1 a2)
    (t1'@(DirectType tn1 q1 a1), t2'@(DirectType tn2 q2 a2)) ->
      case arithmeticConversion tn1 tn2 of
        Just tn -> Right $ DirectType tn (mergeTypeQuals q1 q2) (mergeAttributes a1 a2)
        Nothing -> compositeType t1' t2'
    (t1', t2') -> compositeType t1' t2'

derefType :: Type -> Either String Type
derefType (PtrType t _ _) = Right t
derefType (ArrayType t _ _ _) = Right t
derefType t =
  -- XXX: is it good to use canonicalType here?
  case canonicalType t of
    PtrType t' _ _ -> Right t'
    ArrayType t' _ _ _ -> Right t'
    _ -> Left $ "dereferencing non-pointer: " ++ pType t

varAddrType :: IdentDecl -> Either String Type
varAddrType d =
  do case declStorage d of
       Auto True -> Left "address of register variable"
       _         -> Right ()
     case t of
       ArrayType _ _ q a -> Right $ PtrType t q a
       _                 -> Right $ simplePtr t
  where t = declType d

-- | Get the type of field @m@ of type @t@
fieldType :: (MonadCError m, MonadSymtab m) => NodeInfo -> Ident -> Type -> m Type
fieldType ni m t =
  case canonicalType t of
    DirectType (TyComp ctr) _ _ ->
      do td <- lookupSUE ni (sueRef ctr)
         ms <- tagMembers ni td
         case lookup m ms of
           Just ft -> return ft
           Nothing -> typeError ni $ "field not found: " ++ identToString m
    _t' -> astError ni $
          "field of non-composite type: " ++ identToString m
          ++ ", " ++ pType t

-- | Get all members of a struct, union, or enum, with their
--   types. Collapse fields of anonymous members.
tagMembers :: (MonadCError m, MonadSymtab m) =>
              NodeInfo -> TagDef -> m [(Ident, Type)]
tagMembers ni td =
  case td of
    CompDef (CompType _ _ ms _ _) -> getMembers ms
    EnumDef (EnumType _ es _ _) -> getMembers es
  where getMembers ds =
          do let ts = map declType ds
                 ns = map declName ds
             concat `liftM` mapM (expandAnonymous ni) (zip ns ts)

-- | Expand an anonymous composite type into a list of member names
--   and their associated types.
expandAnonymous :: (MonadCError m, MonadSymtab m) =>
                   NodeInfo -> (VarName, Type)
                -> m [(Ident, Type)]
expandAnonymous ni (NoName, DirectType (TyComp ctr) _ _) =
  lookupSUE ni (sueRef ctr) >>= tagMembers ni
expandAnonymous _ (NoName, _) = return []
expandAnonymous _ (VarName n _, t) = return [(n, t)]

lookupSUE :: (MonadCError m, MonadSymtab m) =>
             NodeInfo -> SUERef -> m TagDef
lookupSUE ni sue =
  do dt <- getDefTable
     case lookupTag sue dt of
       Just (Right td) -> return td
       _               ->
         typeError ni $ "unknown composite type: " ++ (render . pretty) sue

deepTypeAttrs :: (MonadCError m, MonadSymtab m) =>
                 Type -> m Attributes
deepTypeAttrs (DirectType (TyComp (CompTypeRef sue _ ni)) _ attrs) =
  (attrs ++) `liftM` sueAttrs ni sue
deepTypeAttrs (DirectType (TyEnum (EnumTypeRef sue ni)) _ attrs) =
  (attrs ++) `liftM` sueAttrs ni sue
deepTypeAttrs (DirectType _ _ attrs) = return attrs
deepTypeAttrs (PtrType t _ attrs) = (attrs ++) `liftM` deepTypeAttrs t
deepTypeAttrs (ArrayType t _ _ attrs) = (attrs ++) `liftM` deepTypeAttrs t
deepTypeAttrs (FunctionType (FunType t _ _) attrs) =
  (attrs ++) `liftM` deepTypeAttrs t
deepTypeAttrs (FunctionType (FunTypeIncomplete t)  attrs) =
  (attrs ++) `liftM` deepTypeAttrs t
deepTypeAttrs (TypeDefType (TypeDefRef i _ ni) _ attrs) =
  (attrs ++) `liftM` typeDefAttrs ni i

typeDefAttrs :: (MonadCError m, MonadSymtab m) =>
                NodeInfo -> Ident -> m Attributes
typeDefAttrs ni i =
  do dt <- getDefTable
     case lookupIdent i dt of
       Nothing -> astError ni $ "can't find typedef name: " ++ identToString i
       Just (Left (TypeDef _ t attrs _)) -> (attrs ++) `liftM` deepTypeAttrs t
       Just (Right _) -> astError ni $ "not a typedef name: " ++ identToString i

sueAttrs :: (MonadCError m, MonadSymtab m) =>
            NodeInfo -> SUERef -> m Attributes
sueAttrs ni sue =
  do dt <- getDefTable
     case lookupTag sue dt of
       Nothing -> astError ni $ "SUE not found: " ++ render (pretty sue)
       Just (Left _) -> return []
       Just (Right (CompDef (CompType _ _ _ attrs _))) -> return attrs
       Just (Right (EnumDef (EnumType _ _ attrs _))) -> return attrs
