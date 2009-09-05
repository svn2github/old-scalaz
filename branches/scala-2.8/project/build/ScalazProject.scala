import java.io.File
import sbt._
import java.util.jar.Attributes.Name._
import scala.Seq

final class ScalazProject(info: ProjectInfo) extends DefaultProject(info) {
  val servlet = "javax.servlet" % "servlet-api" % "2.5"

  override def compileOrder = CompileOrder.JavaThenScala

  override def compileOptions = target(Target.Java1_5) ::
          Unchecked ::
          super.compileOptions.toList

  override def packageOptions = ManifestAttributes((IMPLEMENTATION_TITLE, "Scalaz"), (IMPLEMENTATION_URL, "http://code.google.com/p/scalaz"), (IMPLEMENTATION_VENDOR, "The Scalaz Project"), (SEALED, "true")) :: Nil

  override def documentOptions = documentTitle("Scalaz " + projectVersion + " API Specification") :: windowTitle("Scalaz " + projectVersion) ::
          super.documentOptions.toList

  val forkedCompilerJar = property[File]
  val forkedLibraryJar = property[File]
  val doFork = propertyOptional[Boolean](false)
  val debugScalac = propertyOptional[Boolean](false)

  override def fork = if (doFork.get.get) Some(new ForkScalaCompiler with ForkScalaRun {
    override def workingDirectory = Some(info.projectPath.asFile)

    override def scalaJars: Iterable[File] = List(forkedCompilerJar.get.get, forkedLibraryJar.get.get)

    override def compileJVMOptions = compileMemoryOptions ++ compileDebugOptions ++ super.compileJVMOptions

    private val compileMemoryOptions = Seq("-Xmx512M", "-Xss2M")

    private def compileDebugOptions = {
      if (debugScalac.get.get)
        Seq("-Xdebug", "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005")
      else
        Seq.empty
    }
  }) else super.fork
}
