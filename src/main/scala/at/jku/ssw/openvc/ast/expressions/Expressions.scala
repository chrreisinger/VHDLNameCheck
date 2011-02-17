package at.jku.ssw.openvc.ast.expressions

import at.jku.ssw.openvc.ast._

abstract sealed class Expression extends Locatable

final case class Term(position: Position, left: Expression, operator: Operators.Term, right: Expression) extends Expression

final case class AggregateExpression(aggregate: Aggregate) extends Expression {
  val position = aggregate.position
}

final case class Relation(position: Position, left: Expression, operator: Operators.Relation, right: Expression) extends Expression

final case class QualifiedExpression(typeName: SelectedName, expression: Expression) extends Expression {
  val position = typeName.position
}

final case class NameExpression(name: Name) extends Expression {
  val position = name.position
}

final case class ShiftExpression(position: Position, left: Expression, operator: Operators.Shift, right: Expression) extends Expression

final case class Factor(position: Position, left: Expression, operator: Operators.Factor, rightOption: Option[Expression] = None) extends Expression {
  def this(position: Position, left: Expression, operator: Operators.Factor) = this (position, left, operator, None)
}

final case class FunctionCallExpression(functionName: SelectedName, parameterAssociationList: Option[AssociationList]) extends Expression {
  val position = functionName.position
}

final case class LogicalExpression(position: Position, left: Expression, operator: Operators.Logical, right: Expression) extends Expression

final case class SimpleExpression(position: Position, signOperator: Option[Operators.Sign], left: Expression, addOperator: Option[Operators.Add], rightOption: Option[Expression])
  extends Expression {
  def this(position: Position, signOperator: Operators.Sign, left: Expression) = this (position, Option(signOperator), left, None, None)

  def this(position: Position, left: Expression, addOperator: Operators.Add, right: Expression) = this (position, None, left, Option(addOperator), Option(right))

  require(addOperator.isDefined == rightOption.isDefined)
}

final case class NewExpression(position: Position, qualifiedExpressionOrSubTypeIndication: Either[Expression, SubTypeIndication]) extends Expression

final case class Literal(position: Position, text: String, literalType: LiteralType)
  extends Expression

final case class PhysicalLiteral(position: Position, text: String, unitName: Identifier, literalType: LiteralType) extends Expression {
  def this(literal: Literal, unitName: Identifier) = this (literal.position, literal.text, unitName, literal.literalType)
}