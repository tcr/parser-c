-- Minimal example: parse a file, and pretty print it again
module Main where
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Text.PrettyPrint.HughesPJ

import Language.C              -- simple API
import Language.C.System.GCC   -- preprocessor used

usageMsg :: String -> String
usageMsg prg = render $
  text "Usage:" <+> text prg <+> hsep (map text ["CPP_OPTIONS","input_file.c"])

main :: IO ()
main = do
    let usageErr = (hPutStrLn stderr (usageMsg "./ParseAndPrint") >> exitWith (ExitFailure 1))
    args <- getArgs
    when (length args < 1) usageErr
    let (opts,input_file) = (init args, last args)

    -- parse
    ast <- errorOnLeftM "Parse Error" $
      parseCFile (newGCC "gcc") Nothing opts input_file
    -- pretty print
    print $ pretty ast

errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) return
errorOnLeftM :: (Show a) => String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg
