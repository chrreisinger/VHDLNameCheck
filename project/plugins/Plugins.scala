import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  //val scalatePlugin = "net.stbbs.yasushi" % "sbt-scalate-plugin" % "1.0"
  lazy val eclipsifyPlugin = "de.element34" % "sbt-eclipsify" % "0.7.0"

  val ideaRepo = "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
  val ideaPlugin = "com.github.mpeltonen" % "sbt-idea-plugin" % "0.2.0"
  val proguardPlugin = "org.scala-tools.sbt" % "sbt-proguard-plugin" % "0.0.+"
}

