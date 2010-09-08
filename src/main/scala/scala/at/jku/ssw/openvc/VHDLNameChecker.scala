/*
 *     OpenVC, an open source VHDL compiler/simulator
 *     Copyright (C) 2010  Christian Reisinger
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package at.jku.ssw.openvc

import at.jku.ssw.openvc.ast.Position
import at.jku.ssw.openvc.ast.declarations.DesignFile
import util.matching.Regex

final class CheckerMessage(val position: Position, val message: String) {
  override def toString = position + " " + message
}

object ASTBuilder {
  import org.antlr.runtime.{ANTLRStringStream, ANTLRInputStream, CharStream, CommonTokenStream}
  import at.jku.ssw.openvc.ast.parser.{VHDLParser, VHDLLexer}
  import java.io.{InputStream, FileInputStream}

  private final class CaseInsensitiveStringStream(input: String) extends ANTLRStringStream(input) {
    override def LA(i: Int): Int = {
      val laToken = super.LA(i)
      if (laToken != 0 && laToken != CharStream.EOF) return Character.toLowerCase(laToken)
      laToken
    }
  }

  private final class CaseInsensitiveInputStream(stream: InputStream) extends ANTLRInputStream(stream) {
    override def LA(i: Int): Int = {
      val laToken = super.LA(i)
      if (laToken != 0 && laToken != CharStream.EOF) return Character.toLowerCase(laToken)
      laToken
    }
  }

  type ASTResult = (DesignFile, Seq[CheckerMessage])

  private def fromCharStream(caseInsensitiveStringStream: CharStream, configuration: VHDLNameChecker.Configuration): ASTResult = {
    val lexer = new VHDLLexer(caseInsensitiveStringStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new VHDLParser(tokens)
    lexer.ams = configuration.amsEnabled
    parser.ams = configuration.amsEnabled
    val designFile = parser.design_file()
    (designFile, parser.syntaxErrors)
  }

  def fromFile(fileName: String, configuration: VHDLNameChecker.Configuration): ASTResult =
    fromCharStream(new CaseInsensitiveInputStream(new FileInputStream(fileName)), configuration)

  def fromText(code: String, configuration: VHDLNameChecker.Configuration): ASTResult =
    fromCharStream(new CaseInsensitiveStringStream(code), configuration)
}

object VHDLNameChecker {
  final class Configuration(val amsEnabled: Boolean, val debug: Boolean, val properties: Map[Class[_], Regex])

  final class CheckResult(val syntaxErrors: Seq[CheckerMessage], val checkErrors: Seq[CheckerMessage], val sourceFile: String)

  private def check(configuration: Configuration, builder: (String, Configuration) => (DesignFile, Seq[CheckerMessage]), source: String, fileName: String): CheckResult = {
    val parseStart = System.currentTimeMillis
    val (designFile, syntaxErrors) = builder(source, configuration)
    val parseTime = System.currentTimeMillis - parseStart

    val semanticCheckStart = System.currentTimeMillis
    val checkErrors = NameChecker(designFile, configuration)
    val semanticCheckTime = System.currentTimeMillis - semanticCheckStart

    if (configuration.debug) {
      println("parse time:" + parseTime)
      println("check time:" + semanticCheckTime)
      println("complete time:" + (System.currentTimeMillis - parseStart))
    }
    new CheckResult(syntaxErrors, checkErrors, fileName)
  }

  def checkFile(configuration: Configuration, file: String): CheckResult = check(configuration, ASTBuilder.fromFile, file, file)

  def checkFileFromText(configuration: Configuration, code: String, file: String): CheckResult = check(configuration, ASTBuilder.fromText, code, file)
}
