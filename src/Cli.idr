||| CLI entry. `testgen <btt-file-location>`.
module Cli

import Data.List
import System
import System.Directory
import System.File
import Testgen

%default covering

main : IO ()
main = do
  args <- drop 1 <$> getArgs
  Just cwd <- currentDir
    | Nothing => die "cannot determine current directory"
  result <- runTestgen cwd args
  when (result.stderr /= "") $ ignore $ fPutStr stderr result.stderr
  if result.exit == 0
    then exitSuccess
    else exitFailure
