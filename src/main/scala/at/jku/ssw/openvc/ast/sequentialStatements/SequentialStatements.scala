package at.jku.ssw.openvc.ast.sequentialStatements

import at.jku.ssw.openvc.ast._
import expressions._

abstract sealed class SequentialStatement extends ASTNode

final case class AssertStatement(position: Position, label: Option[Identifier], condition: Expression, reportExpression: Option[Expression], severityExpression: Option[Expression]) extends SequentialStatement

final case class WaitStatement(position: Position, label: Option[Identifier], sensitivityList: Option[Seq[Name]], untilCondition: Option[Expression], forExpression: Option[Expression]) extends SequentialStatement

final case class NextStatement(position: Position, label: Option[Identifier], loopLabel: Option[Identifier], condition: Option[Expression]) extends SequentialStatement

final case class ExitStatement(position: Position, label: Option[Identifier], loopLabel: Option[Identifier], condition: Option[Expression]) extends SequentialStatement

final case class NullStatement(position: Position, label: Option[Identifier]) extends SequentialStatement

final case class ReportStatement(position: Position, label: Option[Identifier], reportExpression: Expression, severityExpression: Option[Expression]) extends SequentialStatement

final case class ReturnStatement(position: Position, label: Option[Identifier], expression: Option[Expression]) extends SequentialStatement

abstract sealed class AbstractLoopStatement extends SequentialStatement {
  val sequentialStatements: Seq[SequentialStatement]
  val label: Option[Identifier]
}

final case class LoopStatement(position: Position, label: Option[Identifier], sequentialStatements: Seq[SequentialStatement]) extends AbstractLoopStatement

final case class WhileStatement(position: Position, label: Option[Identifier], condition: Expression, sequentialStatements: Seq[SequentialStatement]) extends AbstractLoopStatement

final case class ForStatement(position: Position, label: Option[Identifier], identifier: Identifier, discreteRange: DiscreteRange, sequentialStatements: Seq[SequentialStatement]) extends AbstractLoopStatement

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
  final case class IfThenPart(condition: Expression, statements: Seq[SequentialStatement])
}

final case class IfStatement(position: Position, label: Option[Identifier], ifThenList: Seq[IfStatement.IfThenPart], elseSequentialStatements: Option[Seq[SequentialStatement]]) extends SequentialStatement