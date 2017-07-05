{-# LANGUAGE TemplateHaskell,PatternGuards #-}
-- | Derives 'CNode' instances for language.c
module Data.Derive.CNode(makeCNode) where

{-
-- For all type variables a, we require (CNode a)
-- If we have a data constructor
--   X a_1 .. a_n, and exactly one a_k is a polymorphic variable, then return (nodeInfo a_k)
data Test3 a = A Test1 a Test1 | B a Test2 | C Test1 a deriving (Show {-! ,CNode !-})
-- Else If we have a data constructor
--   X a_1 .. a_n, and exactly one a_k is a Language.C.Data.NodeInfo, then return that a_k
data Test1 = X Int NodeInfo |  Y NodeInfo String | Z Int NodeInfo Integer deriving (Show {-! ,CNode !-})

-- Else If we have a data constructor
--   X a, then return nodeInfo a
data Test2 = U Test1 | V Test1 deriving (Show {-! ,CNode !-})
-- Else Fail
-}
import Language.Haskell.Exts hiding (paren)
import Language.Haskell -- helpers from Derive
import Data.Derive.Internal.Derivation
import Data.Derive.Annotated

makeCNode :: Derivation
makeCNode = derivationCustom "CNode" (runDeriveM . genNodeInst)

nodeInfoTypeName :: [Char]
nodeInfoTypeName = "Language.C.Data.Node.NodeInfo"

genNodeInst :: FullDataDecl -> DeriveM [Decl]
genNodeInst (_,dat) = do
  nodeInfoDecls <- nodeInfoDefs "nodeInfo" dat
  return $
   [ instanceContext ["CNode"] "CNode" dat [ FunBind $ nodeInfoDecls ]
   , instanceContext ["CNode"] "Pos" dat [ FunBind  $ posOfDef "posOf" ]
   ]

posOfDef :: String -> [Match]
posOfDef funName =
  [ funDecl funName [pvar "x"]
                 (app (var "posOf") (paren $ app (var "nodeInfo") (var "x")))
  ]
  where
    var  = Var . qname
    pvar = PVar . Ident

nodeInfoDefs :: String -> DataDecl -> DeriveM [Match]
nodeInfoDefs funName dat = mapM nodeInfoImpl (dataDeclCtors dat) where
    nodeInfoImpl ctor =
        case matchNodeInfo ctor of
            DOk (pat,rhs) ->
                return $ funDecl funName [pat] rhs
            DErr err ->
                fail   $ "Failed to derive NodeInfo for " ++ ctorDeclName ctor ++ ": " ++ err

matchNodeInfo :: CtorDecl -> DeriveM (Pat, Exp)
matchNodeInfo ctor = ctorArgs ctor >>= tryNodeInfoArg
  where
    tryNodeInfoArg args =
        case filter (isNodeInfo.fromBangType.snd) args  of
            []       -> tryDelegate args
            [(ix,_)] -> return $ (matchIndex ctor args ix (PVar (name "n")), Var (qname "n"))
            _        -> fail   $ "More than one NodeInfo type"
        where
            isNodeInfo (TyCon qname) | (Qual _ (Ident "NodeInfo")) <- qname = True
                                     | (UnQual (Ident "NodeInfo")) <- qname = True
                                     | otherwise = False
            isNodeInfo _ = False
    tryDelegate args =
       case args of
           []        -> fail   $ "cannot derive NodeInfo for nullary constructor"
           [_c]      -> return $ (PApp (qname $ ctorDeclName ctor) [PVar (name "d")],
                                  App (Var (qname "nodeInfo")) (Var (qname "d")))
           _xs       -> delegateToPolymorphic "nodeInfo" ctor
    delegateToPolymorphic :: String -> CtorDecl -> DeriveM (Pat,Exp)
    delegateToPolymorphic fun ctor = ctorArgs ctor >>= delegate
      where
        delegate args =
          case filter (isVarName . fromBangType . snd) args of
            []        -> fail   $ "delegateToPolymorphic: no type variable arguments"
            [(ix,_)]  -> return $ (matchIndex ctor args ix (PVar (name "n")),
                                 App (Var (qname fun)) (Var (qname "n")))
            _xs       -> fail   $ "delegateToPolymorphic: More than one type variable argument"

-- ported from TH.Helpers
instanceContext :: [String] -> String -> Decl -> [Decl] -> Decl
instanceContext reqs cls dat defs = InstDecl noLoc Nothing [] ctx className [hed] (map InsDecl defs)
    where
        vars = [Ident ('t' : show i) | i <- [1..dataDeclArity dat]]
        ctx = [ ClassA (qname req) [TyVar var] | req <- reqs, var <- vars]
        className = qname cls
        hed = (if not (null vars) then TyParen else id) $
              tyApp (TyCon $ qname (dataDeclName dat)) (map TyVar vars)

-- remove Bang or unpack annotation
fromBangType :: Type -> Type
fromBangType (TyBang _ ty) = fromBangType ty
fromBangType ty = ty

