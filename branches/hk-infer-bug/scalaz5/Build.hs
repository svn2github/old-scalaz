{-

Depends
* The Haskell Platform http://hackage.haskell.org/platform/
* Lastik http://hackage.haskell.org/package/Lastik


$LastChangedRevision$
$LastChangedDate$
$LastChangedBy$

-}

module Build where

import Lastik.Scala.Scalac
import Lastik.Runner
import Lastik.Output
import Lastik.Directory
import Lastik.Util
import System.FilePath
import System.Cmd
import System.Exit
import Data.List

exampleDir = "src" </> "example"
mainDir = "src" </> "main"
testDir = "src" </> "test"
resourcesDir = "resources"

build = "build"
buildExample = build </> "example"
buildMain = build </> "main"
buildTest = build </> "test"

cp :: String
cp = "classpath" ~?? [buildExample, buildMain, buildTest]

s :: FilePath -> Scalac
s d = scalac {
  directory = Just d,
  deprecation = True
}

main' :: Scalac
main' = s buildMain

main :: IO ExitCode
main = main' +->- [mainDir]

example' :: Scalac
example' = main' >=>=> s buildExample

example :: IO ExitCode
example = main >>>> (example' +->- [exampleDir])

test' :: Scalac
test' = main' >=>=> s buildTest

test :: IO ExitCode
test = main >>>> (test' +->- [testDir])

-- todo scala function in Lastik
scala :: String -> IO ExitCode
scala k = system ("scala " ++ k)

runExample' :: String -> IO ExitCode
runExample' e = example >>>> scala (intercalate " " [cp, e])

runExample :: String -> IO ExitCode
runExample = runExample' . ("scalaz." ++)

allExample :: IO ExitCode
allExample = runExample "Example"

repl :: IO ExitCode
repl = example >>>> test >>>> scala (intercalate " " ["-i initrepl", cp])

clean :: IO ()
clean = rmdir build

