{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CEquiv.hs (Executable)
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
--
-- This module is invoked just like gcc. It preprocesses the two C source files given in the arguments
-- and parses them. Then it compares the ASTs. If CTEST_NON_EQUIV is set, the comparison is expected to fail,
-- otherwise it is expected that the ASTs are equal.
--
-- Tests are logged, and serialized into a result file.
-- If the CEquiv finishes without runtime error, it always returns ExitSuccess.
--
-- see 'TestEnvironment'.
-----------------------------------------------------------------------------
module Main (main)  where
import Control.Monad.State
import System.FilePath (takeBaseName)
import Text.PrettyPrint

import Language.C.Data
import Language.C.Test.Environment
import Language.C.Test.Framework
import Language.C.Test.ParseTests
import Language.C.Test.TestMonad

nonEquivEnvVar :: String 
nonEquivEnvVar = "CTEST_NON_EQUIV"

main :: IO ()
main = defaultMain usage theEquivTest

usage :: Doc
usage =    text "./Equiv [gcc-opts] file.1.(c|hc|i) file.2.(c|hc|i)"
        $$ nest 4 (text "Test Driver: Parses two files and compares the ASTs")
        $$ envHelpDoc [ (nonEquivEnvVar, ("expected that the ASTs aren't equal",Just "False")) ]
        
theEquivTest :: [String] -> TestMonad ()
theEquivTest args =
  case mungeCcArgs args of
    Ignore         -> errorOnInit args $ "No C source file found in argument list: `cc "  ++ unwords args ++ "'"
    Unknown err    -> errorOnInit args $ "Could not munge CC args: " ++ err ++ " in  `cc "++ unwords args ++ "'"
    Groked [f1,f2] gccArgs -> theEquivTest' f1 f2 gccArgs
    Groked cFiles _ -> errorOnInit args $ "Expected two C source files, but found " ++ unwords cFiles
theEquivTest' :: FilePath -> FilePath -> [String] -> TestMonad ()
theEquivTest' f1 f2 gccArgs = do
    dbgMsg $ "Comparing the ASTs of " ++ f1 ++ " and " ++ f2
    expectNonEquiv <- liftIO$ getEnvFlag nonEquivEnvVar
    dbgMsg $ "Expecting that the ASTs " ++ (if expectNonEquiv then " don't match" else "match") ++ ".\n"

    modify $ setTmpTemplate (takeBaseName f1)
    (cFile1, preFile1) <- runCPP f1 gccArgs
    modify $ setTmpTemplate (takeBaseName f2)
    (cFile2, preFile2) <- runCPP f2 gccArgs
    modify $ setTestRunResults (emptyTestResults (takeBaseName (f1 ++ " == " ++ f2)) [cFile1,cFile2])

    let parseTest1 = initializeTestResult (parseTestTemplate { testName = "01-parse" }) [f1]
    let parseTest2 = initializeTestResult (parseTestTemplate { testName = "02-parse" }) [f2]

    modify $ setTmpTemplate (takeBaseName f1)
    parseResult1 <- runParseTest preFile1 (initPos cFile1)
    addTestM $
      setTestStatus parseTest1 $ 
        either (uncurry testFailWithReport) (testOkNoReport . snd) parseResult1
    ast1 <- either (const exitTest) (return . fst) parseResult1
    modify $ setTmpTemplate (takeBaseName f2)
    parseResult2 <- runParseTest preFile2 (initPos cFile2)
    addTestM $
      setTestStatus parseTest2 $ 
        either (uncurry testFailWithReport) (testOkNoReport . snd) parseResult2
    ast2 <- either (const exitTest) (return . fst) parseResult2
    
    -- check equiv
    modify $ setTmpTemplate (takeBaseName f1 ++ "_eq_" ++ takeBaseName f2)
    equivResult <- runEquivTest ast1 ast2
    case expectNonEquiv of
      False ->
        let equivTest = initializeTestResult (equivTestTemplate { testName = "03-equiv" }) [] in
        addTestM . setTestStatus equivTest $
          either (uncurry testFailure) testOkNoReport equivResult        
      True  ->
        let equivTest = initializeTestResult (equivTestTemplate { testName = "03-non-equiv" }) [] in
        addTestM . setTestStatus equivTest $
          either (\(_,mReport) -> testOkUntimed mReport) 
                 (\_ -> testFailNoReport "ASTs are equivalent") equivResult        
