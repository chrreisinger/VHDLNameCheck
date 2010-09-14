package at.jku.ssw.openvc.ast {

import expressions.Expression
import declarations.UseClause

final class Position(val line: Int, val charPosition: Int) {
  override def toString = "Position(" + line + "," + charPosition + ")"
}

trait Locatable {
  val position: Position
}

abstract sealed class ASTNode extends Locatable

abstract sealed class DeclarativeItem extends ASTNode

final class Identifier(val position: Position, val text: String) extends Locatable {
  override val toString: String = this.text

  override def equals(other: Any): Boolean =
    other match {
      case id: Identifier => id.text == this.text
      case _ => false
    }

  override def hashCode = text.hashCode
}

final class Target(val nameOrAggregate: Either[Name, Aggregate])

final class Range(val fromExpression: Expression, val direction: Direction, val toExpression: Expression, val attributeNameOption: Option[Name]) extends Locatable {
  def this(fromExpression: Expression, direction: Direction, toExpression: Expression) = this (fromExpression, direction, toExpression, None)

  val position = attributeNameOption match {
    case None => fromExpression.position
    case Some(attribute) => attribute.position
  }
}

final class DelayMechanism(val delayType: DelayType, val rejectExpression: Option[Expression])

final class DiscreteRange(val rangeOrSubTypeIndication: Either[Range, SubTypeIndication]) extends Locatable {
  val position = rangeOrSubTypeIndication match {
    case Left(range) => range.position
    case Right(subType) => subType.position
  }
}

final class SubTypeIndication(val resolutionFunction: Option[SelectedName], val typeName: SelectedName, val constraint: Option[Either[Range, Seq[DiscreteRange]]]) extends Locatable {
  def this(typeName: SelectedName, constraint: Option[Either[Range, Seq[DiscreteRange]]]) = this (None, typeName, constraint)

  val position = resolutionFunction.getOrElse(typeName).position
}

final class Signature(val parameterList: Option[Seq[SelectedName]], val returnType: Option[SelectedName])

object Aggregate {
  final case class ElementAssociation(choices: Option[Choices], expression: Expression)
}
final case class Aggregate(elements: Seq[Aggregate.ElementAssociation])

object Waveform {
  final class Element(val valueExpression: Expression, val timeExpression: Option[Expression])
}
final class Waveform(val elements: Seq[Waveform.Element]) {
  val isUnaffected = this.elements.isEmpty
}

object Choices {
  final class Choice(val rangeOrExpression: Option[Either[DiscreteRange, Expression]]) {
    def this() = this (None)
    // expression,discreteRange == None => OTHERS
    val isOthers = rangeOrExpression.isEmpty
  }
}
final class Choices(val elements: Seq[Choices.Choice])

object AssociationList {
  final class Element(val formalPart: Option[Name], val actualPart: Option[Expression]) {
    def isActualPartOpen: Boolean = actualPart.isEmpty
  }
}
final class AssociationList(val elements: Seq[AssociationList.Element])

object Name {
  abstract sealed class Part extends Locatable
  final case class IndexPart(indexes: Seq[Expression]) extends Part {
    val position = indexes.head.position
  }
  final case class SelectedPart(identifier: Identifier) extends Part {
    val position = identifier.position
  }
  final case class SlicePart(range: DiscreteRange) extends Part {
    val position = range.position
  }
  final case class AttributePart(signature: Option[Signature], identifier: Identifier, expression: Option[Expression]) extends Part {
    val position = identifier.position
  }
  final case class AssociationListPart(associationList: AssociationList) extends Part {
    val position = null //TODO
  }
}

final class SelectedName(val identifiers: Seq[Identifier]) extends Locatable {
  val position = identifiers.head.position
  override val toString = identifiers.mkString(".")
}

final class Name(val identifier: Identifier, val parts: Seq[Name.Part]) extends Locatable {
  val position = identifier.position
}

object InterfaceList {
  abstract sealed class AbstractInterfaceElement {
    val identifierList: Seq[Identifier]
    val expression: Option[Expression]
    val interfaceMode: Option[InterfaceMode]
    val subType: SubTypeIndication
  }
  final case class InterfaceConstantDeclaration(identifierList: Seq[Identifier], subType: SubTypeIndication, expression: Option[Expression]) extends AbstractInterfaceElement {
    val interfaceMode = Some(InterfaceMode.IN)
  }

  final case class InterfaceVariableDeclaration(identifierList: Seq[Identifier], interfaceMode: Option[InterfaceMode], subType: SubTypeIndication, expression: Option[Expression]) extends AbstractInterfaceElement

  final case class InterfaceSignalDeclaration(identifierList: Seq[Identifier], interfaceMode: Option[InterfaceMode], subType: SubTypeIndication, bus: Boolean, expression: Option[Expression]) extends AbstractInterfaceElement

  final case class InterfaceFileDeclaration(identifierList: Seq[Identifier], subType: SubTypeIndication) extends AbstractInterfaceElement {
    val expression: Option[Expression] = None
    val interfaceMode = Some(InterfaceMode.IN)
  }

}
final class InterfaceList(val elements: Seq[InterfaceList.AbstractInterfaceElement])

final class BlockConfigurationSpecification(val nameOrLabel: Either[SelectedName, (Identifier, Option[Either[DiscreteRange, Expression]])]) //SelectedName,(label,blockConfigureIndex)

final class BlockConfiguration(val blockConfigSpec: BlockConfigurationSpecification, val useClauses: Seq[UseClause], val configurations: Seq[AnyRef])

final class ComponentConfiguration(val componentSpecification: AnyRef, val bindingIndication: Option[AnyRef], val blockConfiguration: Option[BlockConfiguration])

package sequentialStatements {

import at.jku.ssw.openvc.ast.expressions._

abstract sealed class SequentialStatement extends ASTNode

final case class AssertStatement(position: Position, label: Option[Identifier], condition: Expression, reportExpression: Option[Expression], severityExpression: Option[Expression]) extends SequentialStatement

final case class WaitStatement(position: Position, label: Option[Identifier], sensitivityList: Option[Seq[Name]], untilCondition: Option[Expression], forExpression: Option[Expression]) extends SequentialStatement

final case class NextStatement(position: Position, label: Option[Identifier], loopLabel: Option[Identifier], condition: Option[Expression]) extends SequentialStatement

final case class ExitStatement(position: Position, label: Option[Identifier], loopLabel: Option[Identifier], condition: Option[Expression]) extends SequentialStatement

final case class NullStatement(position: Position, label: Option[Identifier]) extends SequentialStatement

final case class ReportStatement(position: Position, label: Option[Identifier], reportExpression: Expression, severityExpression: Option[Expression]) extends SequentialStatement

final case class ReturnStatement(position: Position, label: Option[Identifier], expression: Option[Expression]) extends SequentialStatement

abstract sealed class AbstractLoopStatement extends SequentialStatement {
  val sequentialStatementList: Seq[SequentialStatement]
  val label: Option[Identifier]
}

final case class LoopStatement(position: Position, label: Option[Identifier], sequentialStatementList: Seq[SequentialStatement]) extends AbstractLoopStatement

final case class WhileStatement(position: Position, label: Option[Identifier], condition: Expression, sequentialStatementList: Seq[SequentialStatement]) extends AbstractLoopStatement

final case class ForStatement(position: Position, label: Option[Identifier], identifier: Identifier, discreteRange: DiscreteRange, sequentialStatementList: Seq[SequentialStatement]) extends AbstractLoopStatement

abstract sealed class SignalAssignmentStatement extends SequentialStatement

final case class SimpleSignalAssignmentStatement(position: Position, label: Option[Identifier], target: Target, delayMechanism: Option[DelayMechanism], waveForm: Waveform) extends SignalAssignmentStatement

abstract sealed class VariableAssignmentStatement extends SequentialStatement

final case class SimpleVariableAssignmentStatement(position: Position, label: Option[Identifier], target: Target, expression: Expression) extends VariableAssignmentStatement

final case class ProcedureCallStatement(label: Option[Identifier], procedureName: SelectedName, parameterAssociationList: Option[AssociationList]) extends SequentialStatement {
  val position = procedureName.position
}

object CaseStatement {
  final class When(val choices: Choices, val statements: Seq[SequentialStatement])
}
final case class CaseStatement(position: Position, label: Option[Identifier], expression: Expression, alternatives: Seq[CaseStatement.When]) extends SequentialStatement

object IfStatement {
  final class IfThenPart(val condition: Expression, val statements: Seq[SequentialStatement])
}

final case class IfStatement(position: Position, label: Option[Identifier], ifThenList: Seq[IfStatement.IfThenPart], elseSequentialStatementList: Option[Seq[SequentialStatement]]) extends SequentialStatement
}

package concurrentStatements {

import at.jku.ssw.openvc.ast.expressions._
import at.jku.ssw.openvc.ast.sequentialStatements.SequentialStatement

abstract sealed class ConcurrentStatement extends ASTNode

abstract sealed class ConcurrentSignalAssignmentStatement extends ConcurrentStatement {
  val postponed: Boolean
  val label: Option[Identifier]
  val guarded: Boolean
  val target: Target
  val delayMechanism: Option[DelayMechanism]
}

object ConcurrentConditionalSignalAssignment {
  final class When(val waveForm: Waveform, val condition: Expression)
}
final case class ConcurrentConditionalSignalAssignment(position: Position, label: Option[Identifier], postponed: Boolean, target: Target, guarded: Boolean,
                                                       delayMechanism: Option[DelayMechanism], alternatives: Seq[ConcurrentConditionalSignalAssignment.When]) extends ConcurrentSignalAssignmentStatement

object ConcurrentSelectedSignalAssignment {
  final class When(val waveForm: Waveform, val choices: Choices)
}
final case class ConcurrentSelectedSignalAssignment(position: Position, label: Option[Identifier], postponed: Boolean, expression: Expression, target: Target,
                                                    guarded: Boolean, delayMechanism: Option[DelayMechanism], alternatives: Seq[ConcurrentSelectedSignalAssignment.When])
        extends ConcurrentSignalAssignmentStatement

final case class ConcurrentProcedureCallStatement(label: Option[Identifier], postponed: Boolean, procedureName: SelectedName, parameterAssociationList: Option[AssociationList])
        extends ConcurrentStatement {
  val position = procedureName.position
}

final case class ConcurrentAssertionStatement(position: Position, label: Option[Identifier], postponed: Boolean, condition: Expression, reportExpression: Option[Expression], severityExpression: Option[Expression]) extends ConcurrentStatement

final case class IfGenerateStatement(position: Position, label: Option[Identifier], condition: Expression, declarativeItems: Seq[DeclarativeItem], statementList: Seq[ConcurrentStatement]) extends ConcurrentStatement

final case class ForGenerateStatement(position: Position, label: Option[Identifier], loopIdentifier: Identifier, discreteRange: DiscreteRange, declarativeItems: Seq[DeclarativeItem],
                                      statementList: Seq[ConcurrentStatement]) extends ConcurrentStatement

final case class ComponentInstantiationStatement(position: Position, label: Identifier, componentType: ComponentType, name: SelectedName, architectureIdentifier: Option[Identifier],
                                                 genericAssociationList: Option[AssociationList], portAssociationList: Option[AssociationList]) extends ConcurrentStatement

final case class ProcessStatement(position: Position, label: Option[Identifier], postponed: Boolean, sensitivityList: Option[Seq[Name]], declarativeItems: Seq[DeclarativeItem],
                                  sequentialStatementList: Seq[SequentialStatement]) extends ConcurrentStatement

final case class BlockStatement(position: Position, label: Option[Identifier], guardExpression: Option[Expression], genericInterfaceList: Option[InterfaceList],
                                genericAssociationList: Option[AssociationList], portInterfaceList: Option[InterfaceList], portAssociationList: Option[AssociationList],
                                declarativeItems: Seq[DeclarativeItem], statementList: Seq[ConcurrentStatement]) extends ConcurrentStatement
}

package declarations {

import at.jku.ssw.openvc.ast.expressions._
import at.jku.ssw.openvc.ast.sequentialStatements.SequentialStatement
import at.jku.ssw.openvc.ast.concurrentStatements.ConcurrentStatement

final case class AliasDeclaration(position: Position, identifier: Identifier, subType: Option[SubTypeIndication], name: Name, signature: Option[Signature]) extends DeclarativeItem

final case class AttributeSpecification(position: Position, identifier: Identifier, entityList: Either[Seq[(Identifier, Option[Signature])], Identifier], entityClass: EntityClass, expression: Expression) extends DeclarativeItem

final case class AttributeDeclaration(position: Position, identifier: Identifier, typeName: SelectedName) extends DeclarativeItem

abstract sealed class ObjectDeclaration extends DeclarativeItem {
  val identifierList: Seq[Identifier]
  val subType: SubTypeIndication
}

final case class VariableDeclaration(position: Position, shared: Boolean, identifierList: Seq[Identifier], subType: SubTypeIndication, initialValueExpression: Option[Expression])
        extends ObjectDeclaration

final case class ConstantDeclaration(position: Position, identifierList: Seq[Identifier], subType: SubTypeIndication, defaultExpression: Option[Expression])
        extends ObjectDeclaration

final case class FileDeclaration(position: Position, identifierList: Seq[Identifier], subType: SubTypeIndication, fileOpenKindExpression: Option[Expression], fileLogicalName: Option[Expression])
        extends ObjectDeclaration

final case class SignalDeclaration(position: Position, identifierList: Seq[Identifier], subType: SubTypeIndication, signalType: Option[SignalType], defaultExpression: Option[Expression])
        extends ObjectDeclaration

final case class ComponentDeclaration(position: Position, identifier: Identifier, genericInterfaceList: Option[InterfaceList], portInterfaceList: Option[InterfaceList])
        extends DeclarativeItem

final case class SubTypeDeclaration(position: Position, identifier: Identifier, subTypeIndication: SubTypeIndication) extends DeclarativeItem

abstract sealed class SubProgramDeclaration extends DeclarativeItem {
  val parameterInterfaceList: Option[InterfaceList]
  val identifier: Identifier
}

final case class FunctionDeclaration(position: Position, pure: Boolean, identifier: Identifier, parameterInterfaceList: Option[InterfaceList], returnType: SelectedName)
        extends SubProgramDeclaration

final case class ProcedureDeclaration(position: Position, identifier: Identifier, parameterInterfaceList: Option[InterfaceList])
        extends SubProgramDeclaration

final case class UseClause(position: Position, useList: Seq[SelectedName]) extends DeclarativeItem

final case class DesignFile(designUnits: Seq[DesignUnit]) extends ASTNode {
  lazy val position = designUnits.head.position
}

final case class DesignUnit(position: Position, libraries: Seq[Identifier], useClauses: Seq[UseClause], libraryUnit: Option[LibraryUnit]) extends ASTNode

abstract sealed class LibraryUnit extends ASTNode {
  val declarativeItems: Seq[DeclarativeItem]
  val identifier: Identifier
  val position = identifier.position
}

final case class ConfigurationDeclaration(identifier: Identifier, declarativeItems: Seq[DeclarativeItem], entityName: SelectedName, blockConfiguration: BlockConfiguration)
        extends LibraryUnit

final case class ArchitectureDeclaration(identifier: Identifier, declarativeItems: Seq[DeclarativeItem], entityName: SelectedName,
                                         concurrentStatements: Seq[ConcurrentStatement])
        extends LibraryUnit

final case class EntityDeclaration(identifier: Identifier, genericInterfaceList: Option[InterfaceList], portInterfaceList: Option[InterfaceList],
                                   declarativeItems: Seq[DeclarativeItem], concurrentStatements: Seq[ConcurrentStatement])
        extends LibraryUnit

final case class PackageDeclaration(identifier: Identifier, declarativeItems: Seq[DeclarativeItem])
        extends LibraryUnit

final case class PackageBodyDeclaration(identifier: Identifier, declarativeItems: Seq[DeclarativeItem])
        extends LibraryUnit

abstract sealed class SubProgramDefinition extends DeclarativeItem {
  val parameterInterfaceList: Option[InterfaceList]
  val identifier: Identifier
}

final case class FunctionDefinition(position: Position, pure: Boolean, identifier: Identifier, parameterInterfaceList: Option[InterfaceList], returnType: SelectedName,
                                    declarativeItems: Seq[DeclarativeItem], sequentialStatementList: Seq[SequentialStatement])
        extends SubProgramDefinition

final case class ProcedureDefinition(position: Position, identifier: Identifier, parameterInterfaceList: Option[InterfaceList], declarativeItems: Seq[DeclarativeItem],
                                     sequentialStatementList: Seq[SequentialStatement])
        extends SubProgramDefinition

final case class ConfigurationSpecification(position: Position) extends DeclarativeItem

final case class GroupDeclaration(position: Position, identifier: Identifier, groupTemplateName: SelectedName, constituentList: Seq[Either[Name, Identifier]]) extends DeclarativeItem

object GroupTemplateDeclaration {
  final class Element(val entityClass: EntityClass, val box: Boolean)
}
final case class GroupTemplateDeclaration(position: Position, identifier: Identifier, elements: Seq[GroupTemplateDeclaration.Element]) extends DeclarativeItem

final case class DisconnectionSpecification(position: Position, signalListOrIdentifier: Either[Seq[SelectedName], Identifier], typeName: SelectedName, timeExpression: Expression) extends DeclarativeItem

abstract sealed class AbstractTypeDeclaration extends DeclarativeItem {
  val identifier: Identifier
}

final case class IncompleteTypeDeclaration(position: Position, identifier: Identifier) extends AbstractTypeDeclaration

final case class IntegerOrFloatingPointTypeDefinition(position: Position, identifier: Identifier, range: Range) extends AbstractTypeDeclaration

final case class AccessTypeDefinition(position: Position, identifier: Identifier, subType: SubTypeIndication) extends AbstractTypeDeclaration

object RecordTypeDefinition {
  final class Element(val identifierList: Seq[Identifier], val subType: SubTypeIndication)
}
final case class RecordTypeDefinition(position: Position, identifier: Identifier, elements: Seq[RecordTypeDefinition.Element]) extends AbstractTypeDeclaration

object PhysicalTypeDefinition {
  final class Element(val identifier: Identifier, val literal: PhysicalLiteral)
}

final case class PhysicalTypeDefinition(position: Position, identifier: Identifier, range: Range, baseIdentifier: Identifier, elements: Seq[PhysicalTypeDefinition.Element]) extends AbstractTypeDeclaration

final case class FileTypeDefinition(position: Position, identifier: Identifier, typeName: SelectedName) extends AbstractTypeDeclaration

abstract sealed class AbstractArrayTypeDefinition extends AbstractTypeDeclaration {
  val subType: SubTypeIndication
}

final case class UnconstrainedArrayTypeDefinition(position: Position, identifier: Identifier, dimensions: Seq[SelectedName], subType: SubTypeIndication) extends AbstractArrayTypeDefinition

final case class ConstrainedArrayTypeDefinition(position: Position, identifier: Identifier, dimensions: Seq[DiscreteRange], subType: SubTypeIndication) extends AbstractArrayTypeDefinition

final case class EnumerationTypeDefinition(position: Position, identifier: Identifier, elements: Seq[Identifier]) extends AbstractTypeDeclaration

final case class ProtectedTypeBodyDeclaration(position: Position, identifier: Identifier, declarativeItems: Seq[DeclarativeItem]) extends AbstractTypeDeclaration

final case class ProtectedTypeDeclaration(position: Position, identifier: Identifier, declarativeItems: Seq[DeclarativeItem]) extends AbstractTypeDeclaration
}
}