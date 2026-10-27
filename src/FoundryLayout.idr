||| Foundry project layout observed from the invocation cwd.
|||
||| $$
||| \mathrm{readLayout}(d) =
||| \begin{cases}
||| \mathrm{Right}(\mathrm{testDir}(\tau)) & \text{if } d/\texttt{foundry.toml}=\tau \\
||| \mathrm{Right}(\texttt{test}) & \text{if } \tau \text{ has no test key} \\
||| \mathrm{Left}(\mathrm{MissingFoundryToml}(\texttt{foundry.toml})) & \text{otherwise}
||| \end{cases}
||| $$
|||
||| types.toml: scaffold.FoundryLayout
module FoundryLayout

import Data.List
import Data.String
import System.File

%default total

||| Why layout could not be read from `cwd`.
public export
data LayoutError : Type where
  MissingFoundryToml : (namedFile : String) -> LayoutError

public export
Eq LayoutError where
  MissingFoundryToml a == MissingFoundryToml b = a == b

public export
Show LayoutError where
  show (MissingFoundryToml namedFile) =
    "MissingFoundryToml " ++ show namedFile

||| Test-directory layout taken from `foundry.toml`.
public export
record FoundryLayout where
  constructor MkFoundryLayout
  testDir : String

public export
Eq FoundryLayout where
  MkFoundryLayout a == MkFoundryLayout b = a == b

public export
Show FoundryLayout where
  show (MkFoundryLayout testDir) = "MkFoundryLayout " ++ show testDir

quotedValue : String -> String
quotedValue s =
  let t = trim s
      cs = unpack t
  in case cs of
       '"' :: rest => pack (takeWhile (/= '"') rest)
       '\'' :: rest => pack (takeWhile (/= '\'') rest)
       _ => t

keyEnded : List Char -> Bool
keyEnded [] = True
keyEnded (c :: _) = not (isAlpha c) && not (isDigit c) && c /= '_'

parseTestDirLine : String -> Maybe String
parseTestDirLine raw =
  let t = trim raw
      rest = drop (length "test") (unpack t)
  in if isPrefixOf "test" t && keyEnded rest
       then case dropWhile (/= '=') rest of
              '=' :: val => Just (quotedValue (pack val))
              _ => Nothing
       else Nothing

testDirFromToml : String -> String
testDirFromToml toml =
  case mapMaybe parseTestDirLine (lines toml) of
    (d :: _) => d
    [] => "test"

||| Read `foundry.toml` from `cwd` only (no parent search).
export
covering
readLayout : HasIO io => (cwd : String) -> io (Either LayoutError FoundryLayout)
readLayout cwd = do
  let path = cwd ++ "/foundry.toml"
  True <- exists path
    | False => pure (Left (MissingFoundryToml "foundry.toml"))
  Right contents <- readFile path
    | Left _ => pure (Left (MissingFoundryToml "foundry.toml"))
  pure (Right (MkFoundryLayout (testDirFromToml contents)))
