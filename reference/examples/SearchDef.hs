-- Simple example demonstrating the syntax - semantic interplay: search and print definitions
module Main where
import System.Environment
import Control.Arrow
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map

import Language.C              -- simple API
import Language.C.Analysis     -- analysis API
import Language.C.System.GCC   -- preprocessor used

main :: IO ()
main = do
    let usage = error "Example Usage: ./ShowDef '((struct|union|enum) tagname|typename|objectname)' -I/usr/include my_file.c"
    args <- getArgs
    when (length args < 2) usage

    -- get cpp options and input file
    let (searchterm:args') = args
    let (opts,c_file) = (init &&& last) args'

    -- parse
    ast <- parseCFile (newGCC "gcc") Nothing opts c_file
            >>= checkResult "[parsing]"
    (globals,_warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast
    let defId = searchDef globals searchterm
    -- traverse the AST and print decls which match
    case defId of
      Nothing -> print "Not found"
      Just def_id -> printDecl def_id ast
    where
    checkResult :: (Show a) => String -> (Either a b) -> IO b
    checkResult label = either (error . (label++) . show) return

    printDecl :: NodeInfo -> CTranslUnit -> IO ()
    printDecl def_id (CTranslUnit decls _) =
      let decls' = filter (maybe False (posFile (posOfNode def_id) ==).fileOfNode) decls in
      mapM_ (printIfMatch def_id) (zip decls' (map Just (tail decls') ++ [Nothing]))
    printIfMatch def (decl,Just next_decl) | posOfNode def >= posOf decl &&
                                             posOfNode def < posOf next_decl = (print . pretty) decl
                                           | otherwise = return ()
    printIfMatch def (decl, Nothing) | posOfNode def >= posOf decl = (print . pretty) decl
                                     | otherwise = return ()
    searchDef globs term =
      case analyseSearchTerm term of
        Left tag -> fmap nodeInfo (Map.lookup tag (gTags globs))
        Right ident ->     fmap nodeInfo (Map.lookup ident (gObjs globs))
                       <|> fmap nodeInfo (Map.lookup ident (gTypeDefs globs))
                       <|> fmap nodeInfo (Map.lookup (NamedRef ident) (gTags globs))
    analyseSearchTerm term =
      case words term of
        [tag,name] | tag `elem` (words "struct union enum") -> Left $ NamedRef (internalIdent name)
        [ident] -> Right (internalIdent ident)
        _ -> error "bad search term"
