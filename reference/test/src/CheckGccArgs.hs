{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CCheckGccArgs
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
--
-- Check if the given gcc args are fine to perform a parse test.
-- Essentially a 'one-liner', used by cc-wrapper.
-----------------------------------------------------------------------------
module Main (main)
where
import System.Environment
import System.Exit
import Language.C.Test.Environment

main :: IO ()
main = do
  args <- getArgs
  case mungeCcArgs args of
    Ignore     -> exitWith (ExitFailure 1)
    Unknown _  -> exitWith (ExitFailure 1)
    Groked [cfile] _ | cfile == "conftest.c" -> exitWith (ExitFailure 1) -- exclude ./configure stuff
                     | otherwise             -> exitWith ExitSuccess
    Groked _ _ -> exitWith (ExitFailure 1)