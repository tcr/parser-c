{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CParse.hs (Executable)
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
--
-- This module is invoked just like gcc. It preprocesses the C source file given in the arguments
-- and parses it. If CTEST_NON_PARSE is set, it is expected that parsing failes, otherwise it expects
-- that parsing succeeds.
--
-- Tests are logged, and serialized into a result file.
-- If the CParse finishes without runtime error, it always returns ExitSuccess.
--
-- see 'TestEnvironment'.
-----------------------------------------------------------------------------
module Main (main)  where
import Control.Monad.State
import System.FilePath (takeBaseName)
import Text.PrettyPrint

import Language.C.Data.Position
import Language.C.Test.Environment
import Language.C.Test.Framework
import Language.C.Test.ParseTests
import Language.C.Test.TestMonad

nonParseEnvVar :: String
nonParseEnvVar = "CTEST_NON_PARSE"

main :: IO ()
main = defaultMain usage theParseTest

usage :: Doc
usage =    text "./CParse [gcc-opts] file.(c|hc|i)"
        $$ nest 4 (text "Test Driver: Parses the given source file")
        $$ envHelpDoc [ (nonParseEnvVar, ("expected that the parse fails",Just "False")) ]

theParseTest :: [String] -> TestMonad ()
theParseTest args =
  case mungeCcArgs args of
    Ignore         -> errorOnInit args $ "No C source file found in argument list: `cc "  ++ unwords args ++ "'"
    Unknown err    -> errorOnInit args $ "Could not munge CC args: " ++ err ++ " in  `cc "++ unwords args ++ "'"
    Groked [origFile] gccArgs -> theParseTest' origFile gccArgs
    Groked cFiles _ -> errorOnInit args $ "More than one source file given: " ++ unwords cFiles

theParseTest' :: FilePath -> [String] -> TestMonad ()
theParseTest' origFile gccArgs = do
    modify $ setTmpTemplate (takeBaseName origFile)

    expectNonParse <- liftIO$ getEnvFlag nonParseEnvVar
    dbgMsg $ "Expecting that the C source file " ++ (if expectNonParse then " doesn't parse" else "parses") ++ ".\n"

    (cFile, preFile) <- runCPP origFile gccArgs
    modify $ setTestRunResults (emptyTestResults (takeBaseName origFile) [cFile])
    parseResult <- runParseTest preFile (initPos cFile)
    case expectNonParse of
      True ->
        let parseTest1 = initializeTestResult (parseTestTemplate { testName = "01-fail-parse" }) [origFile] in
        addTestM $
          setTestStatus parseTest1 $
            either (\(_,report) -> testOkUntimed (Just report)) -- no timing available
                   (\_ -> testFailNoReport "parse should fail, but succeeded")  parseResult
      False  ->
        let parseTest1 = initializeTestResult (parseTestTemplate { testName = "01-parse" }) [origFile] in
        addTestM $
          setTestStatus parseTest1 $
            either (\(errMsg,report) -> testFailWithReport errMsg report)
                   (\(_,perf) -> testOkNoReport perf) parseResult
