module Build where

import Lastik.Scala.Scalac
import Lastik.Runner
import Lastik.Output
import Lastik.Util
import System.FilePath

exampleDir = "src" </> "example"
mainDir = "src" </> "main"
testDir = "src" </> "test"
resourcesDir = "resources"

buildExample = "build" </> "example"
buildMain = "build" </> "main"
buildTest = "build" </> "test"

main' = scalac {
  directory = Just buildMain
}

main = main' +->- [mainDir]

example' = main' >=>=> scalac {
  directory = Just buildExample
}

example = main >>>> (example' +->- [exampleDir])

test' = main' >=>=> scalac {
  directory = Just buildTest
}

test = main >>>> (test' +->- [testDir])
