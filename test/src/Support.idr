module Support

import Data.List
import Data.String
import System
import System.Directory
import System.File

%default covering

export
runIO : IO a -> a
runIO = unsafePerformIO

export
must : String -> Either FileError a -> IO a
must label (Left err) = die (label ++ ": " ++ show err)
must _ (Right x) = pure x

export
mkTempDir : IO String
mkTempDir = do
  (out, 0) <- run "mktemp -d /tmp/testgen-XXXXXX"
    | (_, n) => die ("mktemp failed: " ++ show n)
  pure (trim out)

export
writeText : String -> String -> IO ()
writeText path contents = must ("writeFile " ++ path) !(writeFile path contents)

export
readText : String -> IO String
readText path = must ("readFile " ++ path) !(readFile path)

export
repoRoot : IO String
repoRoot = do
  (out, 0) <- run "git rev-parse --show-toplevel"
    | _ => die "git rev-parse --show-toplevel failed"
  pure (trim out)

export
fooBtt : IO String
fooBtt = do
  root <- repoRoot
  readText (root ++ "/playground/.spec/Foo.spec.btt")

export
withEnv : String -> String -> IO a -> IO a
withEnv var val act = do
  old <- getEnv var
  ignore $ setEnv var val True
  result <- act
  case old of
    Nothing => ignore $ unsetEnv var
    Just prev => ignore $ setEnv var prev True
  pure result

export
whichProg : String -> IO String
whichProg name = do
  (out, 0) <- run ("which " ++ name)
    | _ => die ("which " ++ name ++ " failed")
  pure (trim out)

||| A PATH directory that contains only the named executables.
export
pathOnly : List String -> IO String
pathOnly names = do
  dir <- mkTempDir
  bins <- traverse whichProg names
  ignore $ for_ (zip names bins) $ \(name, src) =>
    system ("ln -sfn '" ++ src ++ "' '" ++ dir ++ "/" ++ name ++ "'")
  pure dir

export
linkFoundryLibs : (dir : String) -> IO ()
linkFoundryLibs dir = do
  root <- repoRoot
  ignore $ system ("ln -sfn '" ++ root ++ "/lib' '" ++ dir ++ "/lib'")
  ignore $ system ("ln -sfn '" ++ root ++ "/solidity' '" ++ dir ++ "/solidity'")

export
writeRepoToml : (dir : String) -> IO ()
writeRepoToml dir = do
  root <- repoRoot
  writeText (dir ++ "/foundry.toml") !(readText (root ++ "/foundry.toml"))

export
linkFoundryProject : (dir : String) -> IO ()
linkFoundryProject dir = do
  writeRepoToml dir
  linkFoundryLibs dir

export
importLines : String -> List String
importLines src =
  filter (\l => isPrefixOf "import " (trim l)) (lines src)

export
solNames : List String -> List String
solNames = filter (isSuffixOf ".sol")

export
tomlWithTestDir : String -> String
tomlWithTestDir testDir =
  unlines
    [ "[profile.default]"
    , "src = \"src\""
    , "test = \"" ++ testDir ++ "\""
    ]

export
tomlWithoutTestKey : String
tomlWithoutTestKey =
  unlines
    [ "[profile.default]"
    , "src = \"src\""
    ]

export
bulloakFooSource : String
bulloakFooSource = """
// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.0;

contract FooTest {
    modifier whenStuffIsCalled() {
        _;
    }

    function test_RevertWhen_AConditionIsMet() external whenStuffIsCalled {
        // It should revert.
        //     Because we shouldn't allow it.
    }
}
"""

export
twoImportSource : String
twoImportSource = """
pragma solidity 0.8.0;

import {Foo} from "./Foo.sol";
import {Bar} from "./Bar.sol";

contract FooTest {}
"""

export
compilingTestSource : String
compilingTestSource = """
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.26;

import {PlankTestBase} from "solidity/PlankTestBase.sol";

contract FooTest {
    function test_ok() external {}
}
"""

export
uncompilableSource : String
uncompilableSource = """
pragma solidity ^0.8.26;
contract Broken {
    function nope() external {
        notAFunction();
    }
}
"""
