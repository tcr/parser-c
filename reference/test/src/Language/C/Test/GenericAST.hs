-----------------------------------------------------------------------------
-- |
-- Module      :  GenericAST.hs
-- Copyright   :  (c) Benedikt Huber Wed May 28 09:18:07 CEST 2008
-- Stability   : Prototype !!
--
-- Generic ASTs
-- Note that it seems to be clever to use generics here, as long as the AST
-- isn't set in stone - no need to update when the AST changes.
-----------------------------------------------------------------------------
module Language.C.Test.GenericAST where
import Data.Generics
import Text.PrettyPrint

import Language.C

-- | Generic AST
data GenAST =   GNode Constr [GenAST]
              | GNested [GenAST]
              | GLeaf GenLeaf 
              | GIgnore 
              deriving (Show,Eq)
instance Pretty GenAST where
  pretty (GNode constr sub) = 
    text (show constr) $$ nest 2 (vcat $ map pretty sub)
  pretty (GNested sub) =
    text "-" $$ (nest 2 $ (vcat $ map pretty sub))
  pretty (GLeaf l) = text (show l)
  pretty GIgnore = text ""
  
data GenLeaf = GIdent Ident |
               GCharLit  Char |
               GStringLit String |
               GIntConst Integer |
               GDoubleConst Double
              deriving (Show,Eq,Ord)
-- | Convert C AST into generic AST
mkGenericCAST :: CTranslUnit -> GenAST
mkGenericCAST = toGenericAST . normalizeAST

-- Preprocess AST to normalize blocks
-- compound statements with a single statement in them (no declarations, no local labels) are removed
normalizeAST :: (Data a) => a -> a
normalizeAST = everywhere $ mkT normalizeCompound where
  normalizeCompound :: CStat -> CStat
  normalizeCompound (CCompound [] [CBlockStmt stmt] _) = stmt
  normalizeCompound s = s

-- To build a generic ast, we proceed as follows:
-- If we have a primitive (Ident,Char,String,Integer or Double), we create a generic leaf.
-- If we have a container (Maybe / List), we would like to create a nested node 
--   FIXME: requires SYB with class
-- If we have an Attr, we ignore the datum.
-- If we have an AST Constructor, we get the constructors arguments, 
--   make a list of generic asts and then build the generic ast's node

-- mkQ : (Typeable a, Typeable b) => r -> (b -> r) -> a -> r 
-- extQ: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q 
toGenericAST :: (Data a) => a -> GenAST
toGenericAST = 
         mkAstConNode
  `extQ` mkAstAttr
  `extQ` mkLeaf GIdent
  `extQ` mkLeaf GCharLit
  `extQ` mkLeaf GStringLit
  `extQ` mkLeaf GIntConst
  `extQ` mkLeaf GDoubleConst
  where    
    mkAstConNode :: (Data a) => a -> GenAST
    mkAstConNode v = GNode (toConstr v) . map simplifyNode . filter ( /= GIgnore)  $ gmapQ toGenericAST v
    mkAstAttr :: NodeInfo -> GenAST
    mkAstAttr _ = GIgnore
    mkLeaf :: (a -> GenLeaf) -> (a -> GenAST)
    mkLeaf = (GLeaf .)
    -- bad hack !!! (to do it RIGHT, needs SYB with class)
    simplifyNode (GNode constr [])      | (show constr) == "[]" = GNested []
    simplifyNode (GNode constr [hd,GNested tl]) | (show constr) == "(:)" = GNested (hd:tl)
    simplifyNode (GNode constr [a,b])   | (show constr) == "(,)" = GNested [a,b]
    simplifyNode (GNode constr [a,b,c]) | (show constr) == "(,,)" = GNested [a,b,c]
    simplifyNode (GNode constr [])      | (show constr) == "Nothing" = GNested []    
    simplifyNode (GNode constr [a])     | (show constr) == "Just" = GNested [a]    
    simplifyNode node = node
    -- I think for this one we need SYB 3 (with class)
    --mkAstMaybe :: (Data a) => (Maybe a) -> GenAST
    --mkAstMaybe = maybe (Nested []) (Nested . return . toGenericAST)
    --mkAstList  :: (Data a) => [a] -> GenAST
    --mkAstList  = Nested . map toGenericAST
