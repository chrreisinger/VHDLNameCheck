package at.jku.ssw.openvc.ast.designUnits

import at.jku.ssw.openvc.ast._
import declarativeItems.{DeclarativeItem, UseClause}
import concurrentStatements.ConcurrentStatement

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