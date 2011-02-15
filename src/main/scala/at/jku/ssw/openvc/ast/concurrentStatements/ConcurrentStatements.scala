package at.jku.ssw.openvc.ast.concurrentStatements

import at.jku.ssw.openvc.ast._
import declarations.DeclarativeItem
import expressions._
import sequentialStatements.SequentialStatement

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