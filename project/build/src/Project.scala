import sbt._
import de.element34.sbteclipsify._

class Project(info: ProjectInfo) extends DefaultProject(info) with Eclipsify with IdeaProject with ProguardProject {
  val scalaTestJar = "org.scalatest" % "scalatest" % "1.2"
  val codaRepo = "Coda Hale's Repository" at "http://repo.codahale.com/"
  val fig = "com.codahale" %% "fig" % "1.0.3" // withSources ()
  val scalate = "org.fusesource.scalate" % "scalate-core" % "1.2"
  val slf4j_nop = "org.slf4j" % "slf4j-nop" % "1.6.1"

  override val compileOptions = super.compileOptions ++ (Unchecked :: Deprecation :: /*Optimize ::*/ Nil)
  //overrides the name of the produced jar.
  override val artifactID = "VHDLNameCheck"

  override val mainResources = super.mainResources +++ "NOTICE.txt" +++ "LICENSE.txt" +++ (path("licenses") * "*")

  override val proguardInJars = super.proguardInJars.filter(!_.asFile.getName.contains("scalatest")) +++ Path.fromFile(scalaLibraryJar)

  //override val minJarName = artifactBaseName + ".min.jar"

  val proguardKeepMain = """-keepclasseswithmembers public class at.jku.ssw.openvc.* {
    public static void main(java.lang.String[]);
  }"""

  val proguardKeepRuntime = """-keep public class at.jku.ssw.openvs.*"""

  override val proguardOptions = List(proguardKeepMain, proguardKeepRuntime, "-dontskipnonpubliclibraryclasses", "-dontskipnonpubliclibraryclassmembers",
    "-printconfiguration", "-whyareyoukeeping class org.antlr.runtime.CommonTree")
}