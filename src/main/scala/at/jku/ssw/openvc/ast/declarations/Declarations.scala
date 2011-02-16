package at.jku.ssw.openvc.ast.declarations

import at.jku.ssw.openvc.ast._
import expressions._
import sequentialStatements.SequentialStatement
import concurrentStatements.ConcurrentStatement

abstract sealed class DeclarativeItem extends ASTNode

final case class AliasDeclaration(position: Position, identifier: Identifier, subType: Option[SubTypeIndication], name: Name, signature: Option[Signature]) extends DeclarativeItem

final case class AttributeSpecification(position: Position, identifier: Identifier, entityList: Either[Seq[(Identifier, Option[Signature])], Identifier], entityClass: EntityClass, expression: Expression) extends DeclarativeItem

final case class AttributeDeclaration(position: Position, identifier: Identifier, typeName: SelectedName) extends DeclarativeItem

abstract sealed class ObjectDeclaration extends DeclarativeItem {
  val identifiers: Seq[Identifier]
  val subType: SubTypeIndication
}

final case class VariableDeclaration(position: Position, shared: Boolean, identifiers: Seq[Identifier], subType: SubTypeIndication, initialValueExpression: Option[Expression])
        extends ObjectDeclaration

final case class ConstantDeclaration(position: Position, identifiers: Seq[Identifier], subType: SubTypeIndication, defaultExpression: Option[Expression])
        extends ObjectDeclaration

final case class FileDeclaration(position: Position, identifiers: Seq[Identifier], subType: SubTypeIndication, fileOpenKindExpression: Option[Expression], fileLogicalName: Option[Expression])
        extends ObjectDeclaration

final case class SignalDeclaration(position: Position, identifiers: Seq[Identifier], subType: SubTypeIndication, signalType: Option[SignalType], defaultExpression: Option[Expression])
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
                                    declarativeItems: Seq[DeclarativeItem], sequentialStatements: Seq[SequentialStatement])
        extends SubProgramDefinition

final case class ProcedureDefinition(position: Position, identifier: Identifier, parameterInterfaceList: Option[InterfaceList], declarativeItems: Seq[DeclarativeItem],
                                     sequentialStatements: Seq[SequentialStatement])
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
  final class Element(val identifiers: Seq[Identifier], val subType: SubTypeIndication)
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