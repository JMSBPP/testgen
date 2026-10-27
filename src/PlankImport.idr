||| Insert a `PlankTestBase` import under the last existing import.
|||
||| $$
||| \mathrm{imports}(\mathrm{insertPlankImport}(s))
|||   = \mathrm{imports}(s) \mathbin{+\!\!+} [\ell_{\mathrm{plank}}]
||| $$
|||
||| types.toml: scaffold.PlankImport
module PlankImport

import Data.List
import Data.String

%default total

||| Import line added under the last existing import.
public export
plankImportLine : String
plankImportLine = "import {PlankTestBase} from \"solidity/PlankTestBase.sol\";"

isImport : String -> Bool
isImport l = isPrefixOf "import " (trim l)

isPragma : String -> Bool
isPragma l = isPrefixOf "pragma " (trim l)

indicesWhere : (a -> Bool) -> List a -> List Nat
indicesWhere p xs = go 0 xs
  where
    go : Nat -> List a -> List Nat
    go _ [] = []
    go n (x :: ys) = (if p x then [n] else []) ++ go (S n) ys

insertAfterLast : (String -> Bool) -> String -> List String -> List String
insertAfterLast p line ls =
  case reverse (indicesWhere p ls) of
    [] => line :: ls
    (i :: _) => take (S i) ls ++ [line] ++ drop (S i) ls

||| Rewrite Solidity so `plankImportLine` is the next import after the last
||| existing import (and is present even when there were none).
export
insertPlankImport : String -> String
insertPlankImport src =
  let ls = lines src
  in unlines $
       case reverse (indicesWhere isImport ls) of
         (i :: _) => take (S i) ls ++ [plankImportLine] ++ drop (S i) ls
         [] => insertAfterLast isPragma plankImportLine ls
