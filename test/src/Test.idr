module Test

import BulloakRun
import Data.List
import Data.String
import ForgeCheck
import FoundryLayout
import Hedgehog
import PlankImport
import Support
import System
import System.Directory
import System.File
import Testgen

%hide Prelude.Range
%default covering

--------------------------------------------------------------------------------
-- FoundryLayout
--------------------------------------------------------------------------------

layoutFromToml : String -> Either LayoutError FoundryLayout
layoutFromToml toml = runIO $ do
  dir <- mkTempDir
  writeText (dir ++ "/foundry.toml") toml
  readLayout dir

layoutFromCwd : (setup : String -> IO ()) -> Either LayoutError FoundryLayout
layoutFromCwd setup = runIO $ do
  dir <- mkTempDir
  setup dir
  readLayout dir

foundryLayoutGroup : Group
foundryLayoutGroup =
  MkGroup "FoundryLayout"
    [ ( fromString "uses the toml test directory"
      , property1 $
          layoutFromToml (tomlWithTestDir "tests") ===
            Right (MkFoundryLayout "tests")
      )
    , ( fromString "defaults to test when toml has no test key"
      , property1 $
          layoutFromToml tomlWithoutTestKey ===
            Right (MkFoundryLayout "test")
      )
    , ( fromString "missing foundry.toml names that file"
      , property1 $
          layoutFromCwd (\_ => pure ()) ===
            Left (MissingFoundryToml "foundry.toml")
      )
    , ( fromString "does not search a parent directory for foundry.toml"
      , property1 $
          (runIO $ do
             parent <- mkTempDir
             writeText (parent ++ "/foundry.toml") (tomlWithTestDir "tests")
             ignore $ createDir (parent ++ "/nested")
             readLayout (parent ++ "/nested"))
          === Left (MissingFoundryToml "foundry.toml")
      )
    ]

--------------------------------------------------------------------------------
-- BulloakRun
--------------------------------------------------------------------------------

scaffoldFoo : (String -> String) -> Either BulloakError String
scaffoldFoo mkPath = runIO $ do
  dir <- mkTempDir
  writeText (dir ++ "/Foo.spec.btt") !fooBtt
  runBulloak dir (mkPath dir)

bulloakRunGroup : Group
bulloakRunGroup =
  MkGroup "BulloakRun"
    [ ( fromString "relative and absolute btt paths yield the same FooTest Solidity"
      , property1 $ do
          let rel = scaffoldFoo (const "Foo.spec.btt")
          let absP = scaffoldFoo (\dir => dir ++ "/Foo.spec.btt")
          src <- evalEither rel
          absP === rel
          isInfixOf "contract FooTest" src === True
          isInfixOf "// It should revert." src === True
          isInfixOf "//     Because we shouldn't allow it." src === True
      )
    , ( fromString "missing btt names the path that was passed"
      , property1 $
          (runIO $ do
             dir <- mkTempDir
             runBulloak dir "missing.spec.btt")
          === Left (MissingBtt "missing.spec.btt")
      )
    , ( fromString "PATH without bulloak is MissingBulloak"
      , property1 $
          (runIO $ do
             dir <- mkTempDir
             writeText (dir ++ "/Foo.spec.btt") !fooBtt
             path <- pathOnly ["forge"]
             withEnv "PATH" path (runBulloak dir "Foo.spec.btt"))
          === Left MissingBulloak
      )
    ]

--------------------------------------------------------------------------------
-- PlankImport
--------------------------------------------------------------------------------

modName : Gen String
modName = map ("Mod" ++) (string (linear 1 6) alpha)

importLine : Gen String
importLine = do
  name <- modName
  pure ("import {" ++ name ++ "} from \"./" ++ name ++ ".sol\";")

plankImportGroup : Group
plankImportGroup =
  MkGroup "PlankImport"
    [ ( fromString "adds PlankTestBase under the last import of a bulloak Foo file"
      , property1 $ do
          let out = insertPlankImport bulloakFooSource
          importLines out === [plankImportLine]
          isInfixOf "contract FooTest" out === True
          isInfixOf "// It should revert." out === True
          isInfixOf "//     Because we shouldn't allow it." out === True
      )
    , ( fromString "keeps existing imports in order and inserts after the last"
      , property1 $ do
          let out = insertPlankImport twoImportSource
          importLines out ===
            [ "import {Foo} from \"./Foo.sol\";"
            , "import {Bar} from \"./Bar.sol\";"
            , plankImportLine
            ]
          isInfixOf "contract FooTest {}" out === True
      )
    , ( fromString "for any import list, PlankTestBase is the next import after the last"
      , property $ do
          imports <- forAll (list (linear 0 5) importLine)
          let src =
                unlines
                  ( "pragma solidity 0.8.0;"
                  :: ""
                  :: imports
                  ++ ["", "contract C {}"]
                  )
          importLines (insertPlankImport src) === imports ++ [plankImportLine]
      )
    ]

--------------------------------------------------------------------------------
-- ForgeCheck
--------------------------------------------------------------------------------

checkInProject : (String -> IO String) -> Either ForgeError ()
checkInProject writeTest = runIO $ do
  dir <- mkTempDir
  linkFoundryProject dir
  ignore $ createDir (dir ++ "/test")
  testPath <- writeTest dir
  checkForge dir testPath

forgeCheckGroup : Group
forgeCheckGroup =
  MkGroup "ForgeCheck"
    [ ( fromString "succeeds only for a forge-buildable path"
      , property1 $ do
          let good =
                checkInProject $ \dir => do
                  let path = "test/Foo.spec.t.sol"
                  writeText (dir ++ "/" ++ path) compilingTestSource
                  pure path
          let bad =
                checkInProject $ \dir => do
                  let path = "test/Broken.t.sol"
                  writeText (dir ++ "/" ++ path) uncompilableSource
                  pure path
          good === Right ()
          case bad of
            Left (BuildFailed reason) =>
              isInfixOf "Error" reason === True
            other =>
              failWith Nothing ("expected BuildFailed, got " ++ show other)
      )
    , ( fromString "PATH without forge is MissingForge"
      , property1 $
          (runIO $ do
             dir <- mkTempDir
             linkFoundryProject dir
             ignore $ createDir (dir ++ "/test")
             writeText (dir ++ "/test/Foo.spec.t.sol") compilingTestSource
             path <- pathOnly ["bulloak"]
             withEnv "PATH" path (checkForge dir "test/Foo.spec.t.sol"))
          === Left MissingForge
      )
    ]

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------

predictedPath : String -> String
predictedPath testDir = testDir ++ "/Foo.spec.t.sol"

cliOnFoo :
     (mkArg : String -> String)
  -> IO (String, TestgenResult, Maybe String)
cliOnFoo mkArg = do
  dir <- mkTempDir
  linkFoundryProject dir
  writeText (dir ++ "/Foo.spec.btt") !fooBtt
  result <- runTestgen dir [mkArg dir]
  let outPath = dir ++ "/" ++ predictedPath "test"
  present <- exists outPath
  contents <- if present then map Just (readText outPath) else pure Nothing
  pure (dir, result, contents)

cliCustomTestDir : IO (TestgenResult, Bool)
cliCustomTestDir = do
  dir <- mkTempDir
  linkFoundryProject dir
  toml <- readText (dir ++ "/foundry.toml")
  writeText (dir ++ "/foundry.toml") (toml ++ "\ntest = \"tests\"\n")
  writeText (dir ++ "/Foo.spec.btt") !fooBtt
  result <- runTestgen dir ["Foo.spec.btt"]
  present <- exists (dir ++ "/tests/Foo.spec.t.sol")
  pure (result, present)

cliTwice : IO (TestgenResult, List String)
cliTwice = do
  dir <- mkTempDir
  linkFoundryProject dir
  writeText (dir ++ "/Foo.spec.btt") !fooBtt
  ignore $ runTestgen dir ["Foo.spec.btt"]
  result <- runTestgen dir ["Foo.spec.btt"]
  names <- case !(listDir (dir ++ "/test")) of
    Left _ => pure []
    Right ns => pure (solNames ns)
  pure (result, names)

cliGroup : Group
cliGroup =
  MkGroup "testgen CLI"
    [ ( fromString "Foo.spec.btt writes a forge-buildable FooTest with comments and PlankTestBase"
      , property1 $ do
          let (_, result, contents) =
                runIO $ cliOnFoo (const "Foo.spec.btt")
          result.exit === 0
          case contents of
            Nothing => failWith Nothing "expected test/Foo.spec.t.sol"
            Just src => do
              isInfixOf plankImportLine src === True
              isInfixOf "contract FooTest" src === True
              isInfixOf "// It should revert." src === True
              isInfixOf "//     Because we shouldn't allow it." src === True
      )
    , ( fromString "relative and absolute btt paths write the same predicted file"
      , property1 $ do
          let (_, rel, relSrc) =
                runIO $ cliOnFoo (const "Foo.spec.btt")
          let (_, abs, absSrc) =
                runIO $ cliOnFoo (\dir => dir ++ "/Foo.spec.btt")
          rel.exit === 0
          abs.exit === 0
          relSrc === absSrc
          case relSrc of
            Nothing => failWith Nothing "expected generated Solidity"
            Just src => isInfixOf plankImportLine src === True
      )
    , ( fromString "uses the toml test directory for the generated file"
      , property1 $ do
          let (result, present) = runIO cliCustomTestDir
          result.exit === 0
          present === True
      )
    , ( fromString "running twice leaves one Solidity file at the bulloak-mapped path"
      , property1 $ do
          let (result, names) = runIO cliTwice
          result.exit === 0
          names === ["Foo.spec.t.sol"]
      )
    , ( fromString "missing foundry.toml exits non-zero and names that file"
      , property1 $ do
          let result = runIO $ do
                dir <- mkTempDir
                runTestgen dir ["Foo.spec.btt"]
          result.exit /== 0
          isInfixOf "foundry.toml" result.stderr === True
      )
    , ( fromString "missing btt exits non-zero and names the path"
      , property1 $ do
          let result = runIO $ do
                dir <- mkTempDir
                writeText (dir ++ "/foundry.toml") tomlWithoutTestKey
                runTestgen dir ["missing.spec.btt"]
          result.exit /== 0
          isInfixOf "missing.spec.btt" result.stderr === True
      )
    , ( fromString "PATH without bulloak exits non-zero and names bulloak"
      , property1 $ do
          let result = runIO $ do
                dir <- mkTempDir
                linkFoundryProject dir
                writeText (dir ++ "/Foo.spec.btt") !fooBtt
                path <- pathOnly ["forge"]
                withEnv "PATH" path (runTestgen dir ["Foo.spec.btt"])
          result.exit /== 0
          isInfixOf "bulloak" result.stderr === True
      )
    , ( fromString "PATH without forge exits non-zero and names forge"
      , property1 $ do
          let result = runIO $ do
                dir <- mkTempDir
                linkFoundryProject dir
                writeText (dir ++ "/Foo.spec.btt") !fooBtt
                path <- pathOnly ["bulloak"]
                withEnv "PATH" path (runTestgen dir ["Foo.spec.btt"])
          result.exit /== 0
          isInfixOf "forge" result.stderr === True
      )
    , ( fromString "rejects --plank instead of treating it as a toggle"
      , property1 $ do
          let result = runIO $ do
                dir <- mkTempDir
                linkFoundryProject dir
                writeText (dir ++ "/Foo.spec.btt") !fooBtt
                runTestgen dir ["--plank", "Foo.spec.btt"]
          result.exit /== 0
          isInfixOf "plank" (toLower result.stderr) === True
      )
    ]

--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

main : IO ()
main =
  test
    [ foundryLayoutGroup
    , bulloakRunGroup
    , plankImportGroup
    , forgeCheckGroup
    , cliGroup
    ]
