package at.jku.ssw.openvc

import at.jku.ssw.openvc.VHDLNameChecker._
import java.io._
import xml.NodeSeq

object Main {
  def main(arguments: Array[String]): Unit = {
    println(" VHDLNameChecker  Copyright (C) 2010  Christian Reisinger")
    try {
      val filter = new FilenameFilter() {
        override def accept(dir: File, name: String): Boolean = (name.endsWith(".vhd") || name.endsWith(".vhdl")) && !name.endsWith("in.vhd")
      }
      val configuration = parseCommandLineArguments(arguments)

      val file = arguments(0)
      def toLines(sourceFile: String) = scala.io.Source.fromFile(sourceFile).getLines().toIndexedSeq
      val result = VHDLNameChecker.checkFile(configuration, file)
      val lines = toLines(file)
      printResultTo(result, new PrintWriter(System.out), Some(lines))
      //printResultToHTML(result, lines, "output.html")
      
      /*
      val files = listFiles(new File("""C:\Users\christian\Desktop\grlib-gpl-1.0.22-b4095\"""), filter, true)
      println("start")
      for (i <- 0 to 10) {
        val start = System.currentTimeMillis
        files.map {
          file =>
          //println(file.getAbsolutePath)
            ASTBuilder.fromFile(file.getAbsolutePath)
        }
        println("time:" + (System.currentTimeMillis - start))
      }
      */
    } catch {
      case e: Exception => e.printStackTrace
    }
  }

  def printResultTo(result: CheckResult, writer: PrintWriter, sourceLinesOption: Option[IndexedSeq[String]]): Unit = {
    import result._
    if (!checkErrors.isEmpty) {
      writer.println(" check errors:" + checkErrors.size)
    }
    def printMessages(prefix: String, messages: Seq[CheckerMessage]): Unit = {
      for (msg <- messages) {
        writer.println(prefix + sourceFile + ": line:" + msg.position.line + " col:" + msg.position.charPosition + " " + msg.message)
        sourceLinesOption.foreach {
          sourceLines =>
            writer.println(sourceLines(msg.position.line - 1).toLowerCase)
            writer.println((" " * (msg.position.charPosition - 1)) + "^")
        }
      }
    }
    printMessages("--", checkErrors)
    writer.flush
  }

  def listFiles(directory: File, filter: FilenameFilter, recursive: Boolean): Seq[File] =
    directory.listFiles().toSeq.flatMap {
      case entry if (filter.accept(directory, entry.getName())) => Seq(entry)
      case entry if (recursive && entry.isDirectory()) => listFiles(entry, filter, recursive)
      case _ => Seq()
    }

  def split(sourceString: String, pos: Int): NodeSeq = {
    val (before, after) = sourceString.splitAt(pos - 1)
    val (identifier, rest) = after.splitAt(after.findIndexOf(_.isWhitespace))
    <p>
      {before}<font color="red">
      {identifier}
    </font>{rest}
    </p>
  }

  def printResultToHTML(result: CheckResult, sourceLines: IndexedSeq[String], file: String): Unit = {
    import org.fusesource.scalate.{TemplateSource, TemplateEngine}
    val engine = new TemplateEngine

    val template = Option(this.getClass.getResource("/templates/Output.scaml")).getOrElse(new File(".\\src\\main\\resources\\templates\\Output.scaml").toURI.toURL)
    val source = TemplateSource.fromURL(template)
    val output = engine.layout(source, Map("result" -> result, "sourceLines" -> sourceLines))
    val writer = new PrintWriter(new FileWriter(file))
    writer.println(output)
    writer.close
    println(output)
  }

  def parseCommandLineArguments(arguments: Array[String]): Configuration = {
    import util.matching.Regex
    def loadConfiguration(file: String): Map[Class[_], Regex] = {
      import _root_.at.jku.ssw.openvc.ast.declarations._
      import _root_.at.jku.ssw.openvc.ast.InterfaceList._
      val classes = List(classOf[PackageBodyDeclaration], classOf[PackageDeclaration], classOf[EntityDeclaration], classOf[ArchitectureDeclaration], classOf[ConfigurationDeclaration],
        classOf[VariableDeclaration], classOf[ConstantDeclaration], classOf[SignalDeclaration], classOf[FileDeclaration], classOf[AbstractTypeDeclaration], classOf[FunctionDefinition],
        classOf[ProcedureDefinition], classOf[ComponentDeclaration], classOf[FunctionDeclaration], classOf[ProcedureDeclaration], classOf[SubTypeDeclaration], classOf[AttributeDeclaration], classOf[AttributeSpecification],
        classOf[AliasDeclaration], classOf[GroupDeclaration], classOf[GroupTemplateDeclaration], classOf[InterfaceVariableDeclaration], classOf[InterfaceSignalDeclaration], classOf[InterfaceFileDeclaration], classOf[InterfaceConstantDeclaration])

      val defaultRexEx = """^[a-z][a-zA-Z0-9]*$"""

      val config = new com.codahale.fig.Configuration(file)
      (for (clazz <- classes) yield (clazz -> config("NameChecker." + clazz.getSimpleName).or(defaultRexEx).r)).toMap
    }
    var debug = false
    for (arg <- arguments) {
      arg match {
        case "-debug" => debug = true
        case _ => println("illegal argument:" + arg)
      }
    }
    new Configuration(debug, loadConfiguration("configuration.json"))
  }

}

/*
         ____________                           ___
        /  _________/\                         /  /\
       /  /\________\/_________   _________   /  / /_________
      /  /_/______   /  ______/\ /_____   /\ /  / //_____   /\
     /________   /\ /  /\_____\/_\____/  / //  / /_\____/  / /
     \______ /  / //  / /      /  ___   / //  / //  ___   / /
   _________/  / //  /_/___   /  /__/  / //  / //  /__/  / /
  /___________/ //________/\ /________/ //__/ //________/ /
  \___________\/ \________\/ \________\/ \__\/ \________\/

            ______________________________________________________________
         | / ____________    ____________________   ___    __________  /\
        _|/ /  _________/\  /_/_/_/_/_/_/_/_/_/_/  /  /\  /_/_/_/_/_/ / /
       | / /  /\________\/_________   _________   /  / /_________    / /
      _|/ /  /_/______   /  ______/\ /_____   /\ /  / //_____   /\  / /
     | / /________   /\ /  /\_____\/_\____/  / //  / /_\____/  / / / /
    _|/  \______ /  / //  / /      /  ___   / //  / //  ___   / / / /
   | / _________/  / //  /_/___   /  /__/  / //  / //  /__/  / / / /
  _|/ /___________/ //________/\ /________/ //__/ //________/ / / /
 | /  \___________\/ \________\/ \________\/ \__\/ \________\/ / /
 |/___________________________________________________________/ /
  \___________________________________________________________\/

                                                    ___
                                                  /  /\
MyClass      _________   _________   _________   /  / /_________
            /  ______/\ /  ______/\ /_____   /\ /  / //_____   /\
           /  /_____ \//  /\_____\/_\____/  / //  / /_\____/  / /
          /_____   /\ /  / /      /  ___   / //  / //  ___   / /
   ___   ______/  / //  /_/___   /  /__/  / //  / //  /__/  / /
  /__/\ /________/ //________/\ /________/ //__/ //________/ /
  \__\/ \________\/ \________\/ \________\/ \__\/ \________\/


         ________  __________________________________________________
      / ____  /\  __________________________________________________
     /_______/  \  __________________________________________________
     \_______\ \ \  __________________________________________________
     / ____  / /\ \  ____________   _____________________   ___   _____
    /_______/ /\/ / /  _________/\   ___________________   /  /\   _____
    \_______\ \/ / /  /\________\/_________   _________   /  / /_________
    / ____  / / / /  /_/______   /  ______/\ /_____   /\ /  / //_____   /\
   /_______/ / / /________   /\ /  /\_____\/_\____/  / //  / /_\____/  / /
   \_______\  /  \______ /  / //  / /      /  ___   / //  / //  ___   / /
   / ____  / / _________/  / //  /_/___   /  /__/  / //  / //  /__/  / /
  /_______/ / /___________/ //________/\ /________/ //__/ //________/ /
  \_______\/  \___________\/ \________\/ \________\/ \__\/ \________\/
    ________________________________________________________________
     ______________________________________________________________
      ____________________________________________________________
      */