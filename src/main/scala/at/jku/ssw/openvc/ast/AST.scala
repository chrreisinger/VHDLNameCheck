package at.jku.ssw.openvc.ast

import expressions.Expression
import declarations.UseClause

final class Position(val line: Int, val charPosition: Int) {
  override def toString = "Position(" + line + "," + charPosition + ")"
}

trait Locatable {
  val position: Position
}

abstract class ASTNode extends Locatable

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

final case class Aggregate(position: Position, elements: Seq[Aggregate.ElementAssociation])

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

  final case class SelectedPart(identifier: Identifier) extends Part {
    val position = identifier.position
  }

  final case class SlicePart(range: DiscreteRange) extends Part {
    val position = range.position
  }

  final case class AttributePart(signature: Option[Signature], identifier: Identifier, expression: Option[Expression]) extends Part {
    val position = identifier.position
  }

  final case class AssociationListPart(position: Position, associationList: AssociationList) extends Part
}

final class SelectedName(val identifiers: Seq[Identifier]) extends Locatable {
  val position = identifiers.head.position
  override val toString = identifiers.mkString(".")
}

final case class Name(identifier: Identifier, parts: Seq[Name.Part]) extends Locatable {
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