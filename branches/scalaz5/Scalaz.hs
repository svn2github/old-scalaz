{-

Depends
* The Haskell Platform http://hackage.haskell.org/platform/
* Lastik http://hackage.haskell.org/package/Lastik


$LastChangedRevision$
$LastChangedDate$
$LastChangedBy$

-}

module Build where

import qualified Lastik.Scala.Scalac as S
import qualified Lastik.Scala.Scaladoc as SD
import Lastik.Runner
import Lastik.Output
import Lastik.Directory
import Lastik.Util
import Lastik.Find
import System.FilePath
import System.Cmd
import System.Process
import System.Exit
import Data.List hiding (find)

exampleDir = "example"  </> "src" </> "main" </> "scala"
mainDir = "core"  </> "src" </> "main" </> "scala"
testDir = "core"  </> "src" </> "test" </> "scala"
resourcesDir = "resources"

build = "build"
buildScaladoc = build </> "scaladoc"
buildClasses = build </> "classes"
buildJar = build </> "jar"

type Version = String

version' :: IO Version
version' = readFile "version"

cp :: String
cp = "classpath" ~?? [buildClasses]

s :: FilePath -> S.Scalac
s d = S.scalac {
  S.directory = Just d,
  S.deprecation = True
}

main' :: S.Scalac
main' = s buildClasses

main :: IO ExitCode
main = main' +->- [mainDir]

example' :: S.Scalac
example' = main' >=>=> s buildClasses

example :: IO ExitCode
example = main >>>> (example' ->- [exampleDir])

test' :: S.Scalac
test' = main' >=>=> s buildClasses

test :: IO ExitCode
test = main >>>> (test' ->- [testDir])

-- todo Update Lastik Scaladoc for Scala 2.8.0
scaladoc' :: Version -> SD.Scaladoc
scaladoc' v = SD.scaladoc {
  SD.directory = Just buildScaladoc,
  SD.etc = Just ("-doc-title \"Scalaz " ++ v ++ " API Specification <div><p><em>Copyright 2008 - 2009 Tony Morris, Runar Bjarnason, Tom Adams, Kristian Domagala, Brad Clow, Ricky Clarkson, Paul Chiusano, Trygve Laugstøl, Nick Partridge, Jason Zaugg</em></p>This software is released under an open source BSD licence.</div>\"")
}

scaladoc :: IO ExitCode
scaladoc = do v <- version'
              scaladoc' v ->- [mainDir]

-- todo scala function in Lastik
scala :: String -> IO ExitCode
scala k = system ("scala " ++ k)

-- todo jar function in Lastik
jar :: String -> IO ExitCode
jar k = system ("jar " ++ k)

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

sversion :: FilePath -> FilePath -> IO ExitCode
sversion c f = do (ec, o, e) <- readProcessWithExitCode c ["-version"] []
                  writeFile f o
                  appendFile f e
                  return ec

sbuildversion :: FilePath -> FilePath -> IO ExitCode
sbuildversion c f = mkdir build >> sversion c (build </> f)

scalaversion :: IO ExitCode
scalaversion = "scala" `sbuildversion` "scalaversion"

scalacversion :: IO ExitCode
scalacversion = "scalac" `sbuildversion` "scalacversion"

scaladocversion :: IO ExitCode
scaladocversion = "scaladoc" `sbuildversion` "scaladocversion"

versions :: IO [ExitCode]
versions = sequence [scalaversion, scalacversion, scaladocversion]

-- Codec.Archive.Zip is too buggy, using jar instead
archive :: IO ExitCode
archive = mkdir buildJar >>
          scalaversion >>
          main >>>>
          example >>>>
          test >>>>
          jar ("-cvfm " ++ buildJar </> "scalaz.jar " ++ resourcesDir </> "META-INF" </> "MANIFEST.MF -C " ++ build ++ " scalaversion -C " ++ buildClasses ++ " .")
