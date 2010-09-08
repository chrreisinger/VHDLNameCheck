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

object Term {
  type Operator = Operator.Value
  object Operator extends Enumeration {
    val MUL, DIV, MOD, REM = Value
  }
}
final case class Term(position: Position, left: Expression, operator: Term.Operator, right: Expression) extends Expression

final case class AggregateExpression(aggregate: Aggregate) extends Expression {
  val position = aggregate.elements.head.choices match {
    case None => aggregate.elements.head.expression.position
    case Some(choices) => choices.elements.head.position
  }
}

final case class TypeCastExpression(expression: Expression) extends Expression {
  val position = expression.position
}

object Relation {
  type Operator = Operator.Value
  object Operator extends Enumeration {
    val EQ, NEQ, LT, LEQ, GT, GEQ = Value
  }
}
final case class Relation(position: Position, left: Expression, operator: Relation.Operator, right: Expression) extends Expression

final case class QualifiedExpression(typeName: SelectedName, expression: Expression) extends Expression {
  val position = typeName.position
}

final case class NameExpression(name: Name) extends Expression {
  val position = name.position
}

object ShiftExpression {
  type Operator = Operator.Value
  object Operator extends Enumeration {
    val SLL, SRL, SLA, SRA, ROL, ROR = Value
  }
}
final case class ShiftExpression(position: Position, left: Expression, operator: ShiftExpression.Operator, right: Expression) extends Expression

object Factor {
  type Operator = Operator.Value
  object Operator extends Enumeration {
    val POW, ABS, NOT = Value //, And, Nand, Or, Nor, Xor, XNor;
  }
}
final case class Factor(position: Position, left: Expression, operator: Factor.Operator, rightOption: Option[Expression] = None) extends Expression

final case class FunctionCallExpression(functionName: SelectedName, parameterAssociationList: Option[AssociationList]) extends Expression {
  val position = functionName.position
}
object LogicalExpression {
  type Operator = Operator.Value
  object Operator extends Enumeration {
    val AND, NAND, OR, NOR, XOR, XNOR = Value
  }
}
final case class LogicalExpression(position: Position, left: Expression, operator: LogicalExpression.Operator, right: Expression) extends Expression

object SimpleExpression {
  type AddOperator = AddOperator.Value
  object AddOperator extends Enumeration {
    val PLUS, MINUS, AMPERSAND = Value
  }
  type SignOperator = SignOperator.Value
  object SignOperator extends Enumeration {
    val PLUS, MINUS = Value
  }
}
final case class SimpleExpression(position: Position, signOperator: Option[SimpleExpression.SignOperator], left: Expression, addOperator: Option[SimpleExpression.AddOperator], rightOption: Option[Expression])
        extends Expression {
  require(addOperator.isDefined == rightOption.isDefined)
}

final case class NewExpression(position: Position, qualifiedExpressionOrSubTypeIndication: Either[Expression, SubTypeIndication]) extends Expression

object Literal {
  type Type = Type.Value
  object Type extends Enumeration {
    val REAL_LITERAL, INTEGER_LITERAL, BASED_LITERAL, CHARACTER_LITERAL, STRING_LITERAL, BIT_STRING_LITERAL, NULL_LITERAL = Value
  }
}

final case class Literal(position: Position, text: String, literalType: Literal.Type)
        extends Expression

final case class PhysicalLiteral(position: Position, text: String, unitName: Identifier, literalType: Literal.Type) extends Expression {
  def this(literal: Literal, unitName: Identifier) = this (literal.position, literal.text, unitName, literal.literalType)
}