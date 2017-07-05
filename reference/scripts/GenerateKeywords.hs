-- Generate lexer patterns (reimplementation)
-- input := tokenlist
-- tokenlist := tokenspec [,\n] tokenlist |
-- tokenspec := (tletters | tctor tletters+) @__?
-- tctor     := ctor | (composite ctor)
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import System.IO
import System.Environment
import Data.List as L
import Data.Ord

parseInput :: String -> [[String]]
parseInput = map (map T.unpack) .
             catMaybes .
             map (parseTokenSpec . T.strip) .
             T.split (','==) . T.intercalate (T.pack ",") .
             T.lines . T.pack

parseTokenSpec t | T.null t = Nothing
                 | T.head t == '(' =
                   let (tokexpr,tokenstr) = T.break (==')') t
                   in Just $ parseTokenSpec' (T.snoc tokexpr ')') (T.words $ T.tail tokenstr)
                 | otherwise =
                   let (tokexpr:tokens) = T.words t
                   in Just $ parseTokenSpec' tokexpr tokens

parseTokenSpec' tokexpr tokenlist =
  case T.unpack (last (tokexpr:tokenlist)) of
    "@__" -> addReservedTokens (parseTokenSpec'' tokexpr (init tokenlist))
    _     -> parseTokenSpec'' tokexpr tokenlist

parseTokenSpec'' tokexpr [] = [tokexpr, tokexpr]
parseTokenSpec'' tokexpr ts = tokexpr : ts

addReservedTokens [tokexpr, tok] = tokexpr : [us `T.append` tok, tok, us `T.append` tok `T.append` us ]
  where us = T.pack "__"
addReservedTokens list = error $ "addReservedTokens" ++ show list

expandInput = sortBy (comparing (dropWhile (=='_') . snd)) . concatMap expand
  where
    expand (t:ts) = [ (t,t') | t' <- ts ]

genOutput (ttok,tstr) =
  "idkwtok " ++ pattern ++ " = tok " ++ (show$length tstr) ++ " " ++ (genTok ttok)
  where
    genTok ts@('(':_) = ts
    genTok (t:ts) = "CTok" ++ (toUpper t : ts)
    pattern = "(" ++ L.intercalate " : " (map charPat tstr) ++ " : [])"
    charPat c = '\'' : c : '\'' : []

run ifile ofile = do
  inp <-readFile ifile
  let tokens = parseInput inp
  withFile ofile WriteMode $ \handle -> do
    hPutStrLn handle $ "-- Tokens: " ++ unwords (concatMap tail tokens)
    mapM_ (hPutStrLn handle) ((map genOutput . expandInput) tokens)

main = do
  arguments <- getArgs
  let (ifile,ofile) =
        case arguments of
          [a,b]-> (a,b)
          _ -> error "Usage: GenerateKeywords.hs tokenlist.txt tokenlist.hs"
  run ifile ofile
