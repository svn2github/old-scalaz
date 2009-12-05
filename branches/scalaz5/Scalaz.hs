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
import Data.Time.Clock
import Data.Time.Calendar
import Codec.Archive.Zip

exampleDir = "example"  </> "src" </> "main" </> "scala"
mainDir = "core"  </> "src" </> "main" </> "scala"
testDir = "core"  </> "src" </> "test" </> "scala"
resourcesDir = "resources"

build = "build"
buildClasses = build </> "classes"
buildScalaz = build </> "scalaz"
buildScaladoc = buildScalaz </> "scaladoc"
buildJar = buildScalaz
jarFile = "scalaz.jar"
buildJar' = buildJar </> jarFile

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
sbuildversion c f = mkdir build >> sversion c (buildScalaz </> f)

scalaversion :: IO ExitCode
scalaversion = "scala" `sbuildversion` "scalaversion"

scalacversion :: IO ExitCode
scalacversion = "scalac" `sbuildversion` "scalacversion"

scaladocversion :: IO ExitCode
scaladocversion = "scaladoc" `sbuildversion` "scaladocversion"

scalazversion :: IO ()
scalazversion = version' >>= writeFile (buildScalaz </> "scalazversion")

time :: IO ()
time = getCurrentTime >>= \t -> writeFile (buildScalaz </> "time") (show (utctDay t) ++ "+" ++ show (utctDayTime t))

versions :: IO [ExitCode]
versions = mkdir buildScalaz >> time >> scalazversion >> sequence [scalaversion, scalacversion, scaladocversion]

-- Codec.Archive.Zip is too buggy, using jar instead
archive :: IO ExitCode
archive = mkdir buildJar >>
          versions >>
          main >>>>
          example >>>>
          test >>>>
          let z = intercalate " " (fmap (\(d, f) -> "-C " ++ d ++ " " ++ f) [(buildScalaz, "scalaversion"), (buildScalaz, "scalazversion"), (buildScalaz, "time"), (buildScalaz, "scalacversion")])
              r = "-cvfm " ++ buildJar' ++ " " ++ resourcesDir </> "META-INF" </> "MANIFEST.MF " ++ z ++ " -C " ++ buildClasses ++ " ."
          in jar r

