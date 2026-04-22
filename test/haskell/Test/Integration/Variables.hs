-- | Variable inspection tests ported from the NodeJS integration testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.Variables (variableTests) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif
import DAP (Variable, variableValue)

variableTests :: TestTree
variableTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.Variables"
    [ testCase "ints and strings should be displayed as values" intsAndStringsTest
    , testCase "arbitrarily deep inspection (issues #8, #9)" deepInspectionTest
    , testCase "string fields of expanded vars are fully evaluated (issue #11)" stringFieldsTest
    , testCase "labeled data structures can be expanded (issue #18)" labeledTest
    , testCase "newtype vars not broken (issue #55)" newtypeTest
    , testCase "dont crash on inspect newtype con (issue #64)" newtypeConTest
    , testCase "expand iteratively (issue #97)" expandIterTest
    , testCase "inspect mutable variables (#92)" mutableVarsTest
    , testCase "inspect fully evaluated type (issue #110)" fullyEvaluatedTest
    , testCase "user-defined custom instance (issue #47a)" customInstanceTest
    , testCase "built-in custom instance (issue #47b)" builtinCustomInstanceTest
    , testCase "hdv with containers (issue #47c)" hdvContainersDepTest
    , testCase "hdv in-memory with containers (issue #47d)" hdvContainersMemTest
    , testCase "hdv in-memory with text (issue #47e)" hdvTextMemTest
    ]

intsAndStringsTest :: Assertion
intsAndStringsTest =
  withTestDAPServer "test/integration/cabal1" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = (mkLaunchConfig test_dir "app/Main.hs")
            { lcEntryArgs = ["some", "args"] }
      hitBreakpointWith cfg 15

      vars <- fetchLocalVars
      let a = vars % "a"
          b = vars % "b"
      a @==? "2"
      b @==? "4"

      -- Strings are forced and displayed whole rather than as a structure
      let c = vars % "c"
      cForced <- forceLazy c
      assertIsString cForced "\"call_fxxx\""

      -- After a variable is forced, a new locals request is done. Check again for c == call_fxxx afterwards
      vars2 <- fetchLocalVars
      let c2 = vars2 % "c"
      assertIsString c2 "\"call_fxxx\""

      disconnect

deepInspectionTest :: Assertion
deepInspectionTest =
  withTestDAPServer "test/integration/simple2" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointWith cfg 19

      locals <- fetchLocalVars
      p <- forceLazy (locals % "p")
      pChild <- expandVar p
      _1 <- forceLazy (pChild % "_1")
      assertIsString _1 "\"d=1\""

      -- Walk the spine from d=2 through d=6
      let walk focus level
            | level > 6 = pure focus
            | otherwise = do
                forced <- forceLazy focus
                children <- expandVar forced
                dChild <- forceLazy (children % "_1")
                assertIsString dChild (T.pack $ "\"d=" ++ show level ++ "\"")
                walk (children % "_2") (level + 1)

      finalFocus <- walk (pChild % "_2") (2 :: Int)

      -- Finally, we should be at the OK constructor
      finalFocus @==? "OK"
      disconnect

stringFieldsTest :: Assertion
stringFieldsTest =
  withTestDAPServer "test/integration/repeat" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointWith cfg 5

      -- Force only the 1st "hello" and check the 2nd is already there.
      -- (Mimics reproducer in #11)
      locals <- fetchLocalVars
      x <- forceLazy (locals % "x")
      xChild <- expandVar x
      -- Force the zeroth element
      _ <- forceLazy (xChild % "0")

      -- Refresh the local variables
      locals2 <- fetchLocalVars
      xChild2 <- expandVar (locals2 % "x")
      let _0v = xChild2 % "0"
          _1v = xChild2 % "1"
      _0v @==? "\"hello\""
      _1v @==? "\"hello\""
      disconnect

labeledTest :: Assertion
labeledTest =
  withTestDAPServer "test/integration/labeled" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointWith cfg 8

      -- Force only the 2nd "hello" and check the third is already there.
      -- It relies on repeat seemingly only re-using every other thunk?!!?
      -- (Mimics reproducer in #11)
      locals <- fetchLocalVars
      x <- forceLazy (locals % "x")
      xChild <- expandVar x
      let sNew = xChild % "new" -- No force!
      sNew @==? "3456"
      sLab <- forceLazy (xChild % "lab")
      assertIsString sLab "\"label\""
      disconnect

newtypeTest :: Assertion
newtypeTest =
  withTestDAPServer "test/integration/T55" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointWith cfg 40

      locals <- fetchLocalVars
      y2 <- forceLazy (locals % "y2")
      y2 @==? "Y2 (MyIntX (X _))"
      y2c <- expandVar y2
      let y21 = y2c % "_1"
      y21 @==? "MyIntX (X _)"
      y21c <- expandVar y21
      let y211 = y21c % "_1"
      y211 @==? "X"
      y211c <- expandVar y211
      y2111 <- forceLazy (y211c % "_1")
      y2111 @==? "MyInt 42"
      disconnect

newtypeConTest :: Assertion
newtypeConTest =
  withTestDAPServer "test/integration/T64" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointWith cfg 4

      modVars <- fetchModuleVars
      let myIntConVars = modVars %% "MyIntCon"
      liftIO $ assertBool ("Expecting: Data constructor ‘MyIntCon’; but got " ++ show myIntConVars)
        (any ((== "Data constructor ‘MyIntCon’") . variableValue) myIntConVars)
      disconnect

expandIterTest :: Assertion
expandIterTest =
  withTestDAPServer "test/integration/T97" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointWith cfg 12

      locals <- fetchLocalVars
      let y = locals % "y"
      -- NOTE: This tests that we don't have to force a single variable
      -- as we expand because the data should be fully forced at this
      -- point.
      yChild <- expandVar y
      let v1 = yChild % "_1"
      v1c <- expandVar v1
      let v11 = v1c % "_1"
      v11 @==? "T97" -- just check
      v11c <- expandVar v11
      let v111 = v11c % "_1"
      v111c <- expandVar v111
      let v1111 = v111c % "_1"
      v1111c <- expandVar v1111
      let v11111 = v1111c % "_1"
      v11111c <- expandVar v11111
      let v111111 = v11111c % "_1"
      assertIsString v111111 "\"hello\""
      disconnect

mutableVarsTest :: Assertion
mutableVarsTest =
  withTestDAPServer "test/integration/T92" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "T92.hs"
      hitBreakpointWith cfg 7

      let getMutVarValue :: String -> TestDAP Variable
          getMutVarValue expectedInner = do
            locals <- fetchLocalVars
            let r = locals % "r"
            r @==? "IORef (STRef (GHC.Prim.MutVar# _))"
            rc <- expandVar r
            let r1 = rc % "_1" -- No force
            r1 @==? "STRef"
            r1c <- expandVar r1
            let r11 = r1c % "_1" -- No force
            r11 @==? T.pack ("GHC.Prim.MutVar# " ++ expectedInner)
            r11c <- expandVar r11
            pure (r11c % "_1")

      m1 <- getMutVarValue "False"
      m1 @==? "False"
      next; next; next
      -- Now we're at the start of the last line, where the ref should be True
      -- Note how we get it from scratch, the content of the ref must be
      -- forced again (the forceLazy call in getMutVarValue)
      m2 <- getMutVarValue "True"
      m2 @==? "True"
      disconnect

fullyEvaluatedTest :: Assertion
fullyEvaluatedTest =
  withTestDAPServer "test/integration/T110" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "T110.hs"
      hitBreakpointWith cfg 11
      locals <- fetchLocalVars
      t <- forceLazy (locals % "t")
      t @==? "T"
      tChild <- expandVar t -- No force needed, is fully evaluated and can expand
      let v1 = tChild % "_1"
          v2 = tChild % "_2"
      v1 @==? "333"
      v2 @==? "34"
      disconnect

customInstanceTest :: Assertion
customInstanceTest =
  withTestDAPServer "test/integration/T47a" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointWith cfg 26
      locals <- fetchLocalVars
      action <- forceLazy (locals % "action")
      action @==? "SDJFLSKDJFLKSDJFLSJDKFL"
      ac <- expandVar action
      f1 <- forceLazy (ac % "field1")
      f1 @==? "\"A33\""
      f2 <- forceLazy (ac % "myfield2")
      f2 @==? "3"
      f3 <- forceLazy (ac % "field3")
      f3 @==? "Y"
      f3c <- expandVar f3
      f31 <- forceLazy (f3c % "_1")
      f31 @==? "\"7\""
      let f4 = ac % "field4"
          f5 = ac % "field5"
      f4 @==? "2345"
      f5 @==? "2345.0"
      disconnect

builtinCustomInstanceTest :: Assertion
builtinCustomInstanceTest =
  withTestDAPServer "test/integration/T47b" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointWith cfg 11
      locals <- fetchLocalVars
      action <- forceLazy (locals % "action")
      action @==? "X"
      ac <- expandVar action
      let v1 = ac % "_1"
      v1 @==? "( , )"
      v1c <- expandVar v1
      v2 <- forceLazy (v1c % "fst")
      v2 @==? "\"A33\""
      let v3 = v1c % "snd"
      v3 @==? "3456.0"
      disconnect

hdvContainersDepTest :: Assertion
hdvContainersDepTest = hdvIntMapTest "test/integration/T47c" "Main.hs" 13

hdvContainersMemTest :: Assertion
hdvContainersMemTest = hdvIntMapTest "test/integration/T47d" "Main.hs" 11

hdvIntMapTest :: FilePath -> FilePath -> Int -> Assertion
hdvIntMapTest dir entry line =
  withTestDAPServer dir [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir entry
      hitBreakpointWith cfg line
      -- Check IntMap custom view
      locals <- fetchLocalVars
      action <- forceLazy (locals % "action")
      action @==? "IntMap"
      ac <- expandVar action
      v1 <- forceLazy (ac % "3")
      v1 @==? "\"one\""
      v2 <- forceLazy (ac % "2")
      v2 @==? "\"two\""
      disconnect

hdvTextMemTest :: Assertion
hdvTextMemTest =
  withTestDAPServer "test/integration/T47e" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointWith cfg 11
      locals <- fetchLocalVars
      action <- forceLazy (locals % "action")
      action @==? "\"this should be displayed as a simple string\""
      disconnect
