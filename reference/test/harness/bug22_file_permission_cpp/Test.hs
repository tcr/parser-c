module Main where
import Control.Monad
import System.Environment
import Language.C
import Language.C.System.GCC
main = do
    input <- getArgs >>= \args ->
                case args of
                    [f] -> return f
                    _ ->   error "Usage: ./Test.hs c-file"
    ast <- parseCFile (newGCC "gcc") Nothing [] input
    case ast of
        Left err  -> error (show err)
        Right ast -> print (pretty ast)
        