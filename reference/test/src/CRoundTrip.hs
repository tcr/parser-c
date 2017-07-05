{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CRoundtrip.hs (executable)
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
--
-- This module is invoked just like gcc. It preprocesses the C source file in the given argument list,
-- parses it, pretty prints it again, and compares the two ASTs.
--
-- Tests are logged, and serialized into a result file.
-- If `CRoundtrip' finishes without runtime error, it always returns ExitSuccess.
--
-- see 'TestEnvironment'.
-----------------------------------------------------------------------------
module Main (main)  where
import Control.Monad.State
import System.FilePath (takeBaseName)
import Text.PrettyPrint
import Language.C
import Language.C.Analysis

import Language.C.Test.Environment
import Language.C.Test.Framework
import Language.C.Test.ParseTests
import Language.C.Test.TestMonad

main :: IO ()
main = defaultMain usage roundtripTest

usage :: Doc
usage = text "./CRoundTrip <gcc-args> file.(c|hc|i)"
        $$ (nest 4 $ text "Roundtrip Test Driver: preprocess, parse, typecheck,  pretty print, compile pretty printed, parse again, compare ASTs")
        $+$ envHelpDoc []

roundtripTest :: [String] -> TestMonad ()
roundtripTest args =
  case mungeCcArgs args of
    Ignore         -> errorOnInit args $ "No C source file found in argument list: `cc "  ++ unwords args ++ "'"
    Unknown err    -> errorOnInit args $ "Could not munge CC args: " ++ err ++ " in  `cc "++ unwords args ++ "'"
    Groked [origFile] gccArgs -> roundtripTest' origFile gccArgs
    Groked cFiles _ -> errorOnInit args $ "More than one c source file given: "++ unwords cFiles

roundtripTest' :: FilePath -> [String] -> TestMonad ()
roundtripTest' origFile gccArgs = do
    modify $ setTmpTemplate (takeBaseName origFile)
    (cFile, preFile) <- runCPP origFile gccArgs
    modify $ setTestRunResults (emptyTestResults (takeBaseName origFile) [cFile])

    -- parse
    let parseTest1 = initializeTestResult (parseTestTemplate { testName = "01-parse" }) [origFile]
    parseResult <- runParseTest preFile (initPos cFile)
    addTestM $
      setTestStatus parseTest1 $
        either (uncurry testFailWithReport) (testOkNoReport . snd) parseResult
    ast <- either (const exitTest) (return . fst) parseResult
    -- typecheck
    let tcTest = initializeTestResult (parseTestTemplate { testName = "02-typecheck"}) [origFile]
    tcResult <- runTypecheckTest ast
    addTestM $
        setTestStatus tcTest $
            either testFailNoReport testOkNoReport tcResult

    -- pretty print
    let prettyTest = initializeTestResult (ppTestTemplate { testName = "03-pretty-print" }) [origFile]
    ((prettyFile,report),metric) <- runPrettyPrint ast
    addTestM $
      setTestStatus prettyTest $
        testOkWithReport metric report

    -- parse again (TODO: factor out code duplication with first parse test)
    let parseTest2 = initializeTestResult (parseTestTemplate { testName = "04-parse-pretty-printed" }) [prettyFile]
    parseResult2 <- runParseTest prettyFile (initPos prettyFile)
    addTestM $
      setTestStatus parseTest2 $
        either (uncurry testFailWithReport) (testOkNoReport . snd) parseResult2
    ast2 <- either (const exitTest) (return . fst) parseResult2

    -- compile
    let compileTest = initializeTestResult (compileTestTemplate { testName = "05-compile"}) [prettyFile]
    compileResult <- runCompileTest ("-fsyntax-only":gccArgs) prettyFile
    addTestM $
      setTestStatus compileTest $
        either (uncurry testFailWithReport) (testOkNoReport . snd) compileResult

    -- check equiv
    let equivTest = initializeTestResult (equivTestTemplate { testName = "06-orig-equiv-pp" }) []
    equivResult <- runEquivTest ast ast2
    addTestM $
      setTestStatus equivTest $
        either (uncurry testFailure) testOkNoReport equivResult
    return ()

-- XXX: TODO: Move to framework ?
runTypecheckTest :: CTranslUnit           -- ^ preprocesed file
                 -> TestMonad (Either String PerfMeasure) -- ^ either errMsg (decls,elapsedTime)
runTypecheckTest ast  = do
  -- typecheck
  dbgMsg $ "Starting Typecheck\n"
  (tcResult, elapsed) <-
    time $ do return $! case runTrav_ (analyseAST ast) of
                            Left errs -> Left (concatMap show errs)
                            Right _   -> Right ()
  -- check error and add test
  dbgMsg $ "TypeCheck result: " ++ show tcResult ++ "\n"
  case tcResult of
    Left err -> return $ Left err
    Right _  -> return $ Right (PerfMeasure (0,elapsed))
