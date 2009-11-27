module Build where

import Lastik.Scala.Scalac
import Lastik.Runner
import Lastik.Output
import Lastik.Util
import System.FilePath
import System.Cmd
import System.Exit
import Data.List

exampleDir = "src" </> "example"
mainDir = "src" </> "main"
testDir = "src" </> "test"
resourcesDir = "resources"

buildExample = "build" </> "example"
buildMain = "build" </> "main"
buildTest = "build" </> "test"

cp :: String
cp = "classpath" ~?? [buildExample, buildMain, buildTest]

main' :: Scalac
main' = scalac {
  directory = Just buildMain
}

main :: IO ExitCode
main = main' +->- [mainDir]

example' :: Scalac
example' = main' >=>=> scalac {
  directory = Just buildExample
}

example :: IO ExitCode
example = main >>>> (example' +->- [exampleDir])

test' :: Scalac
test' = main' >=>=> scalac {
  directory = Just buildTest
}

test :: IO ExitCode
test = main >>>> (test' +->- [testDir])

-- todo scala function in Lastik
scala :: String -> IO ExitCode
scala k = system ("scala " ++ k)

runExample :: String -> IO ExitCode
runExample e = example >>>> scala (intercalate " " [cp, e])

allExample :: IO ExitCode
allExample = runExample "scalaz.Example"

repl :: IO ExitCode
repl = scala (intercalate " " ["-i repl", cp])
