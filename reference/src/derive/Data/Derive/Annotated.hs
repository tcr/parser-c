{-# LANGUAGE TemplateHaskell #-}
-- | Derives 'Annotated' instances for language.c
module Data.Derive.Annotated(
    makeAnnotated,
    -- few misc helpers for my derivations
    isVarName, ctorArgs, selectPolyArg, matchIndex,
    noLoc, funDecl,
    -- a monad with failure (Either String)
    DeriveM(..), runDeriveM
    ) where

{-
-- For a type T a, for each constructor C:
--   If C ~ X a_1 .. a_n, and exactly one a_k is a polymorphic variable, then
--    annotation t@(X a_1 ... a_n) = a_k
--    amap f     t@(X a_1 ... a_n) = X a_1 ... (f a_k) ... a_n
--   If C ~ X t, where t is of type S a, then
--    annotation (X s) = annotation s
--    amap f (X s) = amap f s
--   Else Fail
-- data Test1 a = A Int a String | B a (Test a) (Test a) | C a | D (Test1 a)
-}
import Control.Monad (liftM)
import Language.Haskell.Exts hiding (paren)
import Language.Haskell -- helpers from Derive
import Data.Derive.Internal.Derivation

makeAnnotated :: Derivation
makeAnnotated = derivationCustom "Annotated" (runDeriveM . genAnnotatedInst)

genAnnotatedInst :: FullDataDecl -> DeriveM [Decl]
genAnnotatedInst (_,dat) = do
  let ctors = dataDeclCtors dat
  (annotDecls, amapDecls) <- liftM unzip $ mapM (annotClause "annotation" "amap") ctors
  return [ InstDecl noLoc -- SrcLoc
                    Nothing -- Maybe Overlap
                    [] -- TyVarBind
                    [] -- Context
                    (qname "Annotated") -- QName
                    [TyCon $ qname (dataDeclName dat)] -- [Type]
                    (map InsDecl [ FunBind annotDecls, FunBind amapDecls ]) ] -- [InstDecl]

annotClause :: String -> String -> CtorDecl -> DeriveM (Match, Match)
annotClause annot amap ctor = do
  args <- ctorArgs ctor
  case (selectPolyArg args, selectDelegateArg args) of
    ( DOk (ix,_), DErr _ ) -> return ( funDecl annot  [matchIndex ctor args ix (PVar (name "n"))] (Var (qname "n"))
                                     , funDecl amap [PVar (name f), matchCtor ctor args "a_"] (mapPoly ctor args ix) )
    ( DErr _, DOk _ )      -> return ( funDecl annot  [matchOne ctor "n"] (app (Var (qname annot)) (Var (qname "n")))
                                     , funDecl amap [PVar (name f), matchOne ctor "n"] (amapRec ctor "n") )
    ( DErr m1, DErr m2)    -> fail $ "Deriving Annotation: Constructor has neither exactly one variable type argument, nor"++
                                     "exactly one argument of type (T a). " ++ m1 ++ ". " ++ m2
    ( DOk _, DOk _)        -> fail $ "Internal Error: Constructor has both a variable type argument, and a constructor type argument"
  where
    f = "f"
    argName i = qname ("a_" ++ show i)
    mapPoly ctor args ix = apps (Con (qname $ ctorDeclName ctor)) (map (applyAt ix) args)
    applyAt i (index,_) | index == i = app (Var (qname f)) (Var (argName i))
                        | otherwise  = Var (argName index)
    matchOne ctor var = PApp (qname (ctorDeclName ctor)) [PVar (name var)]
    amapRec ctor var = App (Con (qname (ctorDeclName ctor))) (Paren (apps (Var (qname amap)) [Var (qname f), Var (qname var)]))

-- we do not have source locations when generating code
noLoc :: SrcLoc
noLoc = SrcLoc "<generated>" 0 0

-- whether we have a ctor argument of variable type
isVarName :: Type -> Bool
isVarName (TyVar _) = True
isVarName _         = False

-- remove Bang or unpack annotation
fromBangType :: Type -> Type
fromBangType (TyBang _ ty) = fromBangType ty
fromBangType ty = ty

-- constructor arguments
ctorArgs :: CtorDecl -> DeriveM [(Integer,Type)]
ctorArgs ctor@(Left _)  = return $ zip [(1::Integer)..] $ map snd (ctorDeclFields ctor)
ctorArgs ctor@(Right _) = fail   $ "CNode: GADTs are not supported: " ++ show ctor

selectDelegateArg :: [(Integer, Type)] -> DeriveM Type
selectDelegateArg args =
  case args of
    []       -> fail "Select Delegate Argument: Constructor has no argument"
    [(_,bty)] -> case fromTyParens (fromBangType bty) of
      ty@(TyApp (TyCon _) (TyVar _)) -> return ty
      ty     -> fail $ "Select Delegate Argument: Constructor is not of the form T x: " ++ show ty
    _xs      -> fail "Select Delegate Argument: Constructor has more than one argument"

selectPolyArg :: [(Integer, Type)] -> DeriveM (Integer, Name)
selectPolyArg args =
  case filter (isVarName . fromBangType . snd) args of
        []             -> fail   $ "Select Polymorphic Argument: no type variable arguments in " ++ show args
        [(ix,ty)]      -> return $ (ix,fromTyVar (fromBangType ty))
        _xs            -> fail   $ "Select Polymorphic Argument: More than one type variable argument in " ++ show args
  where fromTyVar (TyVar n) = n

-- a little bit more powerful than simpleFun ;)
funDecl :: String -> [Pat] -> Exp -> Match
funDecl funName patterns rhs = Match noLoc (Ident funName) patterns Nothing (UnGuardedRhs rhs) Nothing

matchCtor :: CtorDecl -> [(Integer, t)] -> String -> Pat
matchCtor ctor ctorArgs varPrefix = PApp (qname (ctorDeclName ctor)) $ map matchArg ctorArgs
  where
    matchArg (ix,_) = PVar (name $ varPrefix ++ show ix)

matchIndex :: (Eq a) => CtorDecl -> [(a, t)] -> a -> Pat -> Pat
matchIndex ctor ctorArgs ix matchPat = PApp (qname (ctorDeclName ctor)) $ map matchArg ctorArgs
  where
    matchArg (ix',_) | ix == ix' = matchPat
                     | otherwise = PWildCard

-- I want to have an error monad, and Monad Either is not available :(
data DeriveM a = DOk a | DErr String
runDeriveM (DOk a)    = Right a
runDeriveM (DErr msg) = Left msg
instance Functor DeriveM where
  fmap f (DOk a) = DOk (f a)
  fmap _ (DErr msg) = DErr msg
instance Applicative DeriveM where
  pure x = DOk x
  mf <*> ma = case (mf, ma) of
    (DOk f, DOk a) -> DOk (f a)
    (DErr msg, _)  -> DErr msg
    (_, DErr msg)  -> DErr msg
instance Monad DeriveM where
  return = DOk
  (>>=) (DErr msg) f = DErr msg
  (>>=) (DOk ok)   f = f ok
  fail msg = DErr msg
