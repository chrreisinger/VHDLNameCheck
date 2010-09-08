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

import at.jku.ssw.openvc.ast._
import at.jku.ssw.openvc.ast.concurrentStatements._
import at.jku.ssw.openvc.ast.sequentialStatements._
import at.jku.ssw.openvc.ast.declarations._
import at.jku.ssw.openvc.VHDLNameChecker.Configuration

object NameChecker {
  def apply(designFile: DesignFile, configuration: Configuration): Seq[CheckerMessage] = {
    val checkerMessages = new collection.mutable.ListBuffer[CheckerMessage]
    acceptNode(designFile)
    return checkerMessages.toList

    implicit def toOption(clazz: Class[_]): Option[Class[_]] = Option(clazz)

    def acceptNode(node: ASTNode): Unit = {
      def acceptList[T <: ASTNode](list: Seq[T]): Unit = list.foreach(acceptNode)
      def check(identifier: Identifier, clazz: Option[Class[_]] = None): Unit = {
        val regex = configuration.properties(clazz.getOrElse(node.getClass))
        regex.findFirstMatchIn(identifier.text) match {
          case None => checkerMessages += new CheckerMessage(identifier.position, "does not match regex:" + regex.pattern)
          case _ =>
        }
      }
      def checkList(identifierList: Seq[Identifier]): Unit = identifierList.foreach(check(_, None))

      def checkInterfaceList(interfaceListOption: Option[InterfaceList]): Unit =
        interfaceListOption.foreach {
          interfaceList =>
            import InterfaceList._
            interfaceList.elements.foreach {
              element =>
                element.identifierList.foreach {
                  identifier =>
                    element match {
                      case variableDeclaration: InterfaceVariableDeclaration => check(identifier, classOf[InterfaceVariableDeclaration])
                      case signalDeclaration: InterfaceSignalDeclaration => check(identifier, classOf[InterfaceSignalDeclaration])
                      case fileDeclaration: InterfaceFileDeclaration => check(identifier, classOf[InterfaceFileDeclaration])
                      case constantDeclaration: InterfaceConstantDeclaration => check(identifier, classOf[InterfaceConstantDeclaration])
                    }
                }
            }
        }
      node match {
        case null =>
        case DesignFile(designUnits) => acceptList(designUnits)
        case designUnit: DesignUnit => designUnit.libraryUnit.foreach(acceptNode)
        case packageBodyDeclaration: PackageBodyDeclaration =>
          check(packageBodyDeclaration.identifier)
          acceptList(packageBodyDeclaration.declarativeItems)
        case packageDeclaration: PackageDeclaration =>
          check(packageDeclaration.identifier)
          acceptList(packageDeclaration.declarativeItems)
        case entityDeclaration: EntityDeclaration =>
          check(entityDeclaration.identifier)
          acceptList(entityDeclaration.declarativeItems)
          acceptList(entityDeclaration.concurrentStatements)
        case architectureDeclaration: ArchitectureDeclaration =>
          check(architectureDeclaration.identifier)
          acceptList(architectureDeclaration.declarativeItems)
          acceptList(architectureDeclaration.concurrentStatements)
        case configurationDeclaration: ConfigurationDeclaration =>
          check(configurationDeclaration.identifier)
          acceptList(configurationDeclaration.declarativeItems)
        //declarative Items
        //handles VariableDeclaration,ConstantDeclaration,SignalDeclaration and FileDeclaration
        case objectDeclaration: ObjectDeclaration => checkList(objectDeclaration.identifierList)
        case typeDeclaration: AbstractTypeDeclaration =>
          check(typeDeclaration.identifier, classOf[AbstractTypeDeclaration])
        /*typeDeclaration match {
          case enumerationType: EnumerationTypeDefinition => //enumerationType.elements.map(id => id.text.replace("'", ""))
          case physicalType: PhysicalTypeDefinition =>
          //physicalType.elements, Map(physicalType.baseIdentifier
          case recordType: RecordTypeDefinition => //recordType.elements.flatMap(_.identifierList)
          case protectedType: ProtectedTypeDeclaration => acceptList(protectedType.declarativeItems)
          case protectedTypeBody: ProtectedTypeBodyDeclaration => acceptList(protectedTypeBody.declarativeItems)
          case typeDef: IncompleteTypeDeclaration =>
          case accessType: AccessTypeDefinition =>
          case fileTypeDefinition: FileTypeDefinition =>
          case integerOrRealType: IntegerOrFloatingPointTypeDefinition =>
          case arrayType: AbstractArrayTypeDefinition =>
        }*/
        case functionDefinition: FunctionDefinition =>
          check(functionDefinition.identifier)
          checkInterfaceList(functionDefinition.parameterInterfaceList)
          acceptList(functionDefinition.declarativeItems)
          acceptList(functionDefinition.sequentialStatementList)
        case procedureDefinition: ProcedureDefinition =>
          check(procedureDefinition.identifier)
          checkInterfaceList(procedureDefinition.parameterInterfaceList)
          acceptList(procedureDefinition.declarativeItems)
          acceptList(procedureDefinition.sequentialStatementList)
        case functionDeclaration: FunctionDeclaration =>
          check(functionDeclaration.identifier)
          checkInterfaceList(functionDeclaration.parameterInterfaceList)
        case procedureDeclaration: ProcedureDeclaration =>
          check(procedureDeclaration.identifier)
          checkInterfaceList(procedureDeclaration.parameterInterfaceList)
        case componentDeclaration: ComponentDeclaration =>
          check(componentDeclaration.identifier)
          checkInterfaceList(componentDeclaration.genericInterfaceList)
          checkInterfaceList(componentDeclaration.portInterfaceList)
        case subTypeDeclaration: SubTypeDeclaration => check(subTypeDeclaration.identifier)
        case attributeDeclaration: AttributeDeclaration => check(attributeDeclaration.identifier)
        case attributeSpec: AttributeSpecification => check(attributeSpec.identifier)
        case aliasDeclaration: AliasDeclaration => check(aliasDeclaration.identifier)
        case groupDeclaration: GroupDeclaration => check(groupDeclaration.identifier)
        case groupTemplateDeclaration: GroupTemplateDeclaration => check(groupTemplateDeclaration.identifier)
        //sequential Statements
        case _: SequentialStatement =>
        //concurrent Statements
        case ifGenerateStatement: IfGenerateStatement =>
          acceptList(ifGenerateStatement.declarativeItems)
          acceptList(ifGenerateStatement.statementList)
        case forGenerateStatement: ForGenerateStatement =>
          acceptList(forGenerateStatement.declarativeItems)
          acceptList(forGenerateStatement.statementList)
        case processStatement: ProcessStatement =>
          acceptList(processStatement.declarativeItems)
          acceptList(processStatement.sequentialStatementList)
        case blockStatement: BlockStatement =>
          checkInterfaceList(blockStatement.genericInterfaceList)
          checkInterfaceList(blockStatement.portInterfaceList)
          acceptList(blockStatement.declarativeItems)
          acceptList(blockStatement.statementList)
        case _ =>
      }
    }
    error("scala bug")
  }
}
