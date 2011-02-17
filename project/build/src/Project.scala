import sbt._
import de.element34.sbteclipsify._

class Project(info: ProjectInfo) extends DefaultProject(info) with Eclipsify with IdeaProject with ProguardProject {
  val scalaTestJar = "org.scalatest" % "scalatest" % "1.3" withSources() withJavadoc()
  val codaRepo = "Coda Hale's Repository" at "http://repo.codahale.com/"
  val fig = "com.codahale" %% "fig" % "1.1.1" withSources()
  val scalate = "org.fusesource.scalate" % "scalate-core" % "1.4.0" withSources() withJavadoc()
  val slf4j_nop = "org.slf4j" % "slf4j-nop" % "1.6.1"

  override val compileOptions = super.compileOptions ++ (Unchecked :: Deprecation :: /*Optimize ::*/ Nil)
  //overrides the name of the produced jar.
  override val artifactID = "VHDLNameCheck"

  override val mainResources = super.mainResources +++ "NOTICE.txt" +++ "LICENSE.txt" +++ (path("licenses") * "*")

  override val mainClass = Some("at.jku.ssw.openvc.Main")

  override val proguardInJars = super.proguardInJars.filter {
    pathFinder =>
      val name = pathFinder.asFile.getName
      !(name.contains("scalatest") || name.contains("sources") || name.contains("javadoc"))
  } +++ scalaLibraryPath

  override def makeInJarFilter(file: String) = file match {
    case "scala-library.jar" => "!META-INF/**" + ",!library.properties"
    case _ => "!META-INF/**"
  }

  //override val minJarName = artifactBaseName + ".min.jar"

  val proguardKeepMain = """-keepclasseswithmembers public class at.jku.ssw.openvc.* {
    public static void main(java.lang.String[]);
  }"""

  val proguardKeepRuntime = """-keep public class at.jku.ssw.openvs.*"""

  override val proguardOptions = List(proguardKeepMain, proguardKeepRuntime, "-dontnote", "-dontskipnonpubliclibraryclasses",
    "-dontskipnonpubliclibraryclassmembers", "-printconfiguration", "-keep class org.codehaus.jackson** { *; }")
}
