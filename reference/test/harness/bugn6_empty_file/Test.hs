-- Bug report:
-- 1. Parse empty file.
-- 2. Then try to print filename.
-- 3. Get error.
-- See attachment.
module Main where
import Language.C
import Language.C.System.GCC

-- Create empty file 'test.c' before
main :: IO ()
main = do
    Right tu <- parseCFilePre "test.c"
    print $ let Just fname = fileOfNode tu in fname