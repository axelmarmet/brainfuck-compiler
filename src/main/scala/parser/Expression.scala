package parser

import scanner.*
import scala.io.Source

abstract class Expression 

case class If(condition : Expression, block : Expression) extends Expression 
case class IfElse(condition : Expression, ifBlock : Expression, elseBlock : Expression) extends Expression
case class Binary(leftOperand : Expression, operator : Operator, rightOperand : Expression) extends Expression
case class Unary(operand : Expression, operator : Operator) extends Expression

abstract class Operator

case class Plus() extends Operator
case class Minus() extends Operator
case class Equal() extends Operator
case class NotEqual() extends Operator

object Parser {
  def parse(tokens : List[Token]) : List[Expression] = tokens match {
    case Nil => Nil
    case IF_TOKEN() :: xs => {
      val (ifExpression, restOfTokens) = parseIfStatement(xs);
      ifExpression :: parse(restOfTokens)
    }

  }
  def splitUntilCorrespondingParenthesis(tokens : List[Token]) : (List[Token], List[Token]) = {
    def inner(tokens : List[Token], acc : List[Token], balance : Int) = tokens match {
      L_PARENTHESIS_TOKEN() :: xs => inner(xs, L_PARENTHESIS_TOKEN() :: acc, balance + 1)
      R_PARENTHESIS_TOKEN() :: xs => if (balance == 0) (acc, xs) else inner(xs, acc, balance - 1)
    }
  }
}