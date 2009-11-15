import sbt._
import sbt.CompileOrder._
import java.util.jar.Attributes.Name._
import java.io.File

abstract class ScalazDefaults(info: ProjectInfo, component: String) extends DefaultProject(info) {
  val scalaTools2_8_0Snapshots = Resolver.url("2.8.0 snapshots") artifacts "http://scala-tools.org/repo-snapshots/org/scala-lang/[module]/2.8.0-SNAPSHOT/[artifact]-[revision].[ext]"

  // This lets you use a local copy of scala. Set build.scala.versions=2.8.0-latest in build.properties.
  // override def localScala = defineScala("2.8.0-latest", Path.userHome / "usr" / "scala-2.8.0.latest" asFile) :: Nil

  override def compileOptions = target(Target.Java1_5) :: Unchecked :: super.compileOptions.toList

  override def packageOptions = ManifestAttributes((IMPLEMENTATION_TITLE, "Scalaz"), (IMPLEMENTATION_URL, "http://code.google.com/p/scalaz"), (IMPLEMENTATION_VENDOR, "The Scalaz Project"), (SEALED, "true")) :: Nil

  override def documentOptions = documentTitle("Scalaz " + component + projectVersion + " API Specification") :: windowTitle("Scalaz " + projectVersion) :: super.documentOptions.toList

  override def managedStyle = ManagedStyle.Maven

  override def packageDocsJar = defaultJarPath("-javadoc.jar")

  override def packageSrcJar = defaultJarPath("-sources.jar")

  val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)

  val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadoc"), Nil, None)

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)
}

final class ScalazProject(info: ProjectInfo) extends ParentProject(info) {
  // Sub-projects
  lazy val core = project("core", "Scalaz Core", new ScalazCoreProject(_))
  lazy val test = project("test", "Scalaz Test", new ScalazTestProject(_), core)

  // Blocked by http://lampsvn.epfl.ch/trac/scala/ticket/2308
  //  lazy val http = project("http", "Scalaz HTTP", new ScalazHttpProject(_), core)
  //  lazy val scapps = project("scapps", "Scalaz Scapps", new ScalazScappsProject(_), core, http)


  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)

  // Use this instead as a test run of pom generation and jar publishing
  // val publishTo = Resolver.file("local-file-repo", new java.io.File("/Users/jason/code/scalaz-maven/snapshots"))

  override def publishAction = task {None}

  // TODO Package the project up
  //packageProjectZip
  //  def extraResources = descendents(info.projectPath / "licenses", "*") +++ "LICENSE" +++ "NOTICE"
  //  override def mainResources = super.mainResources +++ extraResources

  // One-shot build for users building from trunk
  //  lazy val fullBuild = task {None} dependsOn (boot.proguard, main.crossPublishLocal) describedAs
  //      "Builds the loader and builds main sbt against all supported versions of Scala and installs to the local repository."
}

protected final class ScalazCoreProject(info: ProjectInfo) extends ScalazDefaults(info, "Core")

protected final class ScalazTestProject(info: ProjectInfo) extends ScalazDefaults(info, "Test") {
  val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.0-20091106.025327-+" % "1.7-SNAPSHOT" withSources
}

protected final class ScalazHttpProject(info: ProjectInfo) extends ScalazDefaults(info, "HTTP") {
  val servlet = "javax.servlet" % "servlet-api" % "2.5" withSources
}

protected final class ScalazScappsProject(info: ProjectInfo) extends ScalazDefaults(info, "Scapps")
