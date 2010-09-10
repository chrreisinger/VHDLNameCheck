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

package at.jku.ssw.openvc.ast.expressions

import at.jku.ssw.openvc.ast._

abstract sealed class Expression extends Locatable

case object EmptyExpression extends Expression {
  val position = Position.Empty
}

object TermOperator extends Enumeration {
  type Operator = TermOperator.Value
  val MUL, DIV, MOD, REM = Value
}
final case class Term(position: Position, left: Expression, operator: TermOperator.Operator, right: Expression) extends Expression

final case class AggregateExpression(aggregate: Aggregate) extends Expression {
  val position = aggregate.elements.head.choices match {
    case None => aggregate.elements.head.expression.position
  //case Some(choices) => choices.elements.head.position
  }
}

final case class TypeCastExpression(expression: Expression) extends Expression {
  val position = expression.position
}

object RelationOperator extends Enumeration {
  type Operator = RelationOperator.Value
  val EQ, NEQ, LT, LEQ, GT, GEQ = Value
}
final case class Relation(position: Position, left: Expression, operator: RelationOperator.Operator, right: Expression) extends Expression

final case class QualifiedExpression(typeName: SelectedName, expression: Expression) extends Expression {
  val position = typeName.position
}

final case class NameExpression(name: Name) extends Expression {
  val position = name.position
}

object ShiftOperator extends Enumeration {
  type Operator = ShiftOperator.Value
  val SLL, SRL, SLA, SRA, ROL, ROR = Value
}
final case class ShiftExpression(position: Position, left: Expression, operator: ShiftOperator.Operator, right: Expression) extends Expression

object FactorOperator extends Enumeration {
  type Operator = FactorOperator.Value
  val POW, ABS, NOT = Value
}
final case class Factor(position: Position, left: Expression, operator: FactorOperator.Operator, rightOption: Option[Expression] = None) extends Expression {
  def this(position: Position, left: Expression, operator: FactorOperator.Operator) = this (position, left, operator, None)
}

final case class FunctionCallExpression(functionName: SelectedName, parameterAssociationList: Option[AssociationList]) extends Expression {
  val position = functionName.position
}
object LogicalOperator extends Enumeration {
  type Operator = LogicalOperator.Value
  val AND, NAND, OR, NOR, XOR, XNOR = Value
}
final case class LogicalExpression(position: Position, left: Expression, operator: LogicalOperator.Operator, right: Expression) extends Expression

object AddOperator extends Enumeration {
  type Operator = AddOperator.Value
  val PLUS, MINUS, AMPERSAND = Value
}

object SignOperator extends Enumeration {
  type Operator = SignOperator.Value
  val PLUS, MINUS = Value
}

final case class SimpleExpression(position: Position, signOperator: Option[SignOperator.Operator], left: Expression, addOperator: Option[AddOperator.Operator], rightOption: Option[Expression])
        extends Expression {
  def this(position: Position, signOperator: SignOperator.Operator, left: Expression) = this (position, Option(signOperator), left, None, None)

  def this(position: Position, left: Expression, addOperator: AddOperator.Operator, right: Expression) = this (position, None, left, Option(addOperator), Option(right))
  require(addOperator.isDefined == rightOption.isDefined)
}

final case class NewExpression(position: Position, qualifiedExpressionOrSubTypeIndication: Either[Expression, SubTypeIndication]) extends Expression

object LiteralType extends Enumeration {
  type Type = LiteralType.Value
  val REAL_LITERAL, INTEGER_LITERAL, BASED_LITERAL, CHARACTER_LITERAL, STRING_LITERAL, BIT_STRING_LITERAL, NULL_LITERAL = Value
}

final case class Literal(position: Position, text: String, literalType: LiteralType.Type)
        extends Expression

final case class PhysicalLiteral(position: Position, text: String, unitName: Identifier, literalType: LiteralType.Type) extends Expression {
  def this(literal: Literal, unitName: Identifier) = this (literal.position, literal.text, unitName, literal.literalType)
}