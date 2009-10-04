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

  val scalaTools2_8_0Snapshots = Resolver.url("2.8.0 snapshots") artifacts "http://scala-tools.org/repo-snapshots/org/scala-lang/[module]/2.8.0-SNAPSHOT/[artifact]-[revision].[ext]"


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

  // todo configure publishing to scala-tools once credentials for scala-tools are obtained.
  override def managedStyle = ManagedStyle.Maven
  val localFileRepo = Resolver.file("local-file-repo", new java.io.File("/Users/jason/code/scalaz-maven/snapshots")) 
  val publishTo = localFileRepo

  // todo scaladoc blows up with:
  // java.lang.NullPointerException
  //  at scala.tools.nsc.typechecker.Typers$Typer.checkNoDoubleDefsAndAddSynthetics$1(Typers.scala:1866)
  //
  //  override def packageDocsJar = defaultJarPath("-javadoc.jar")
  //  val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadoc"), Nil, None)
  override def packageSrcJar= defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(/*packageDocs,*/ packageSrc)
}
