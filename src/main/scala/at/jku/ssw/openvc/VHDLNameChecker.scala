package at.jku.ssw.openvc

import at.jku.ssw.openvc.ast.Position
import at.jku.ssw.openvc.ast.declarations.DesignFile
import util.matching.Regex

final class CheckerMessage(val position: Position, val message: String) {
  override def toString = position + " " + message
}

object ASTBuilder {
  import java.io.{InputStream, FileInputStream, ByteArrayInputStream}

  def fromInputStream(inputStream: InputStream): DesignFile = {
    val scanner = new Scanner(inputStream)
    val parser = new Parser(scanner)
    parser.VHDL()
  }

  def fromFile(fileName: String): DesignFile = fromInputStream(new FileInputStream(fileName))

  def fromText(code: String): DesignFile = fromInputStream(new ByteArrayInputStream(code.getBytes("utf-8")))

}

object VHDLNameChecker {
  final class Configuration(val debug: Boolean, val properties: Map[Class[_], Regex])

  final class CheckResult(val checkErrors: Seq[CheckerMessage], val sourceFile: String)

  private def check(configuration: Configuration, builder: (String) => DesignFile, source: String, fileName: String): CheckResult = {
    val parseStart = System.currentTimeMillis
    val designFile = builder(source)
    val parseTime = System.currentTimeMillis - parseStart

    val semanticCheckStart = System.currentTimeMillis
    val checkErrors = NameChecker(designFile, configuration)
    val semanticCheckTime = System.currentTimeMillis - semanticCheckStart

    if (configuration.debug) {
      println("parse time:" + parseTime)
      println("check time:" + semanticCheckTime)
      println("complete time:" + (System.currentTimeMillis - parseStart))
    }
    new CheckResult(checkErrors, fileName)
  }

  def checkFile(configuration: Configuration, file: String): CheckResult = check(configuration, ASTBuilder.fromFile, file, file)

  def checkText(configuration: Configuration, code: String, file: String): CheckResult = check(configuration, ASTBuilder.fromText, code, file)
}
