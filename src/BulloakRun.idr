||| Run `bulloak scaffold` and return Solidity source.
|||
||| $$
||| \mathrm{runBulloak}(p) =
||| \begin{cases}
||| \mathrm{Right}(\sigma) & \text{if bulloak scaffolds } p \text{ to } \sigma \\
||| \mathrm{Left}(\mathrm{MissingBtt}(p)) & \text{if } p \text{ does not exist} \\
||| \mathrm{Left}(\mathrm{MissingBulloak}) & \text{if bulloak is not on PATH} \\
||| \mathrm{Left}(\mathrm{BulloakFailed}(r)) & \text{otherwise}
||| \end{cases}
||| $$
|||
||| types.toml: scaffold.BulloakRun
module BulloakRun

import Data.String
import System
import System.Escape
import System.File

%default total

||| Why scaffolding could not produce Solidity.
public export
data BulloakError : Type where
  MissingBtt : (namedPath : String) -> BulloakError
  MissingBulloak : BulloakError
  BulloakFailed : (reason : String) -> BulloakError

public export
Eq BulloakError where
  MissingBtt a == MissingBtt b = a == b
  MissingBulloak == MissingBulloak = True
  BulloakFailed a == BulloakFailed b = a == b
  _ == _ = False

public export
Show BulloakError where
  show (MissingBtt namedPath) = "MissingBtt " ++ show namedPath
  show MissingBulloak = "MissingBulloak"
  show (BulloakFailed reason) = "BulloakFailed " ++ show reason

isAbsolute : String -> Bool
isAbsolute path = isPrefixOf "/" path

resolvePath : (cwd : String) -> (path : String) -> String
resolvePath cwd path = if isAbsolute path then path else cwd ++ "/" ++ path

cmdMissing : (out : String) -> (code : Int) -> Bool
cmdMissing out code =
  code == 127
    || isInfixOf "not found" (toLower out)
    || isInfixOf "cannot find" (toLower out)

||| Scaffold `bttPath` (relative paths resolved against `cwd`) to Solidity.
export
covering
runBulloak :
     HasIO io
  => (cwd : String)
  -> (bttPath : String)
  -> io (Either BulloakError String)
runBulloak cwd bttPath = do
  let resolved = resolvePath cwd bttPath
  True <- exists resolved
    | False => pure (Left (MissingBtt bttPath))
  let cmd =
        "bulloak scaffold --solidity-version 0.8.26 "
          ++ escapeArg resolved
          ++ " 2>&1"
  (out, code) <- run cmd
  if cmdMissing out code
    then pure (Left MissingBulloak)
    else if code == 0
           then pure (Right out)
           else pure (Left (BulloakFailed out))
