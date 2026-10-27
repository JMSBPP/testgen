||| `testgen <btt-file-location>`: layout, scaffold, Plank import, forge gate.
|||
||| $$
||| \mathrm{runTestgen}(d,[p]) = 0
||| \iff
||| \text{a forge-buildable file exists at the bulloak-mapped path under }
||| \mathrm{testDir}(d)
||| $$
|||
||| types.toml: scaffold.Testgen
module Testgen

import BulloakRun
import Data.List1
import Data.String
import ForgeCheck
import FoundryLayout
import PlankImport
import System.Directory
import System.File

%default total

||| CLI observation: process exit and diagnostic text.
public export
record TestgenResult where
  constructor MkTestgenResult
  exit : Int
  stderr : String

public export
Eq TestgenResult where
  MkTestgenResult e1 s1 == MkTestgenResult e2 s2 = e1 == e2 && s1 == s2

public export
Show TestgenResult where
  show (MkTestgenResult exit stderr) =
    "MkTestgenResult " ++ show exit ++ " " ++ show stderr

fail : String -> TestgenResult
fail msg = MkTestgenResult 1 (msg ++ "\n")

ok : TestgenResult
ok = MkTestgenResult 0 ""

isPlankFlag : String -> Bool
isPlankFlag arg = arg == "--plank" || isPrefixOf "--plank=" arg

basename : String -> String
basename path = last (split (== '/') path)

bttToSol : String -> String
bttToSol path =
  let name = basename path
  in if isSuffixOf ".btt" name
       then pack (dropLast 4 (unpack name)) ++ ".t.sol"
       else name ++ ".t.sol"
  where
    dropLast : Nat -> List a -> List a
    dropLast n xs = reverse (drop n (reverse xs))

displayLayout : LayoutError -> String
displayLayout (MissingFoundryToml name) =
  name ++ " not found in the invocation environment"

displayBulloak : BulloakError -> String
displayBulloak (MissingBtt p) = "btt file not found: " ++ p
displayBulloak MissingBulloak = "bulloak is missing from PATH"
displayBulloak (BulloakFailed r) = "bulloak failed: " ++ r

displayForge : ForgeError -> String
displayForge MissingForge = "forge is missing from PATH"
displayForge (BuildFailed r) = "forge build failed: " ++ r

parseArgs : List String -> Either String String
parseArgs args =
  if any isPlankFlag args
    then Left "unexpected flag --plank; PlankTestBase is always applied"
    else case args of
      [p] => Right p
      _ => Left "usage: testgen <btt-file-location>"

||| Run `testgen` as if invoked with `args` (no program name) in `cwd`.
export
covering
runTestgen :
     HasIO io
  => (cwd : String)
  -> (args : List String)
  -> io TestgenResult
runTestgen cwd args =
  case parseArgs args of
    Left msg => pure (fail msg)
    Right btt => do
      Right layout <- readLayout cwd
        | Left err => pure (fail (displayLayout err))
      Right src <- runBulloak cwd btt
        | Left err => pure (fail (displayBulloak err))
      let rewritten = insertPlankImport src
      let rel = layout.testDir ++ "/" ++ bttToSol btt
      ignore $ createDir (cwd ++ "/" ++ layout.testDir)
      Right () <- writeFile (cwd ++ "/" ++ rel) rewritten
        | Left err => pure (fail ("could not write " ++ rel ++ ": " ++ show err))
      Right () <- checkForge cwd rel
        | Left err => pure (fail (displayForge err))
      pure ok
