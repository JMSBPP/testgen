||| Gate success on `forge build` of a generated test path.
|||
||| $$
||| \mathrm{checkForge}(d,p) =
||| \begin{cases}
||| \mathrm{Right}(()) & \text{if forge build of } p \text{ in } d \text{ succeeds} \\
||| \mathrm{Left}(\mathrm{MissingForge}) & \text{if forge is not on PATH} \\
||| \mathrm{Left}(\mathrm{BuildFailed}(r)) & \text{otherwise}
||| \end{cases}
||| $$
|||
||| types.toml: scaffold.ForgeCheck
module ForgeCheck

import Data.String
import System
import System.Escape
import System.File

%default total

||| Why the compile gate failed.
public export
data ForgeError : Type where
  MissingForge : ForgeError
  BuildFailed : (reason : String) -> ForgeError

public export
Eq ForgeError where
  MissingForge == MissingForge = True
  BuildFailed a == BuildFailed b = a == b
  _ == _ = False

public export
Show ForgeError where
  show MissingForge = "MissingForge"
  show (BuildFailed reason) = "BuildFailed " ++ show reason

cmdMissing : (out : String) -> (code : Int) -> Bool
cmdMissing out code =
  code == 127
    || isInfixOf "not found" (toLower out)
    || isInfixOf "cannot find" (toLower out)

||| Run `forge build` on `testPath` using `foundry.toml` from `cwd`.
export
covering
checkForge :
     HasIO io
  => (cwd : String)
  -> (testPath : String)
  -> io (Either ForgeError ())
checkForge cwd testPath = do
  let cmd =
        "forge build --root "
          ++ escapeArg cwd
          ++ " "
          ++ escapeArg testPath
          ++ " 2>&1"
  (out, code) <- run cmd
  if cmdMissing out code
    then pure (Left MissingForge)
    else if code == 0
           then pure (Right ())
           else pure (Left (BuildFailed out))
