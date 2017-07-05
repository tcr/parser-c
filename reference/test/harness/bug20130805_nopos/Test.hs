-- Bug report:
-- 1. run (parseCFile cfile nopos), on a file with syntax errors
-- 2. error message triggers a bug, because no position information is available
module Main where
import Language.C
import Data.ByteString.Char8 as BS (pack)

main :: IO ()
main = do
   let src = BS.pack "int x());"
   case parseC src nopos of
     Left err -> (length (show err)) `seq` (putStrLn "Expected Parse Error")
     Right _ -> putStrLn "Unexpected Parse Success"
   let src2 = BS.pack "int x;"
   case parseC src2 nopos of
     Left _ -> putStrLn "Unexpected Parse Error"
     Right ok -> print (prettyUsingInclude ok)