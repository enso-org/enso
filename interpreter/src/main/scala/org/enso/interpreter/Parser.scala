package org.enso.interpreter

import scala.util.parsing.combinator._
import scala.collection.JavaConverters._

trait AstExpressionVisitor[+T] {
  def visitLong(l: Long): T
  def visitArithOp(op: String, left: AstExpression, right: AstExpression): T
  def visitForeign(lang: String, code: String): T
  def visitVariable(name: String): T

  def visitFunction(
    arguments: java.util.List[String],
    statements: java.util.List[AstExpression],
    retValue: AstExpression
  ): T

  def visitApplication(
    function: AstExpression,
    arguments: java.util.List[AstExpression]
  ): T

  def visitIf(
    cond: AstExpression,
    ifTrue: AstExpression,
    ifFalse: AstExpression
  ): T

  def visitAssignment(varName: String, expr: AstExpression): T

  def visitPrint(body: AstExpression): T
}

trait AstGlobalScopeVisitor[+T] {

  def visitGlobalScope(
    bindings: java.util.List[AstAssignment],
    expression: AstExpression
  ): T
}

case class AstGlobalScope(
  bindings: List[AstAssignment],
  expression: AstExpression)
    extends {

  def visit[T](visitor: AstGlobalScopeVisitor[T]): T =
    visitor.visitGlobalScope(bindings.asJava, expression)
}

sealed trait AstExpression {
  def visit[T](visitor: AstExpressionVisitor[T]): T
}

case class AstLong(l: Long) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitLong(l)
}

case class AstArithOp(op: String, left: AstExpression, right: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitArithOp(op, left, right)
}

case class AstForeign(lang: String, code: String) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitForeign(lang, code)
}

case class AstVariable(name: String) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitVariable(name)
}

case class AstApply(fun: AstExpression, args: List[AstExpression])
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitApplication(fun, args.asJava)
}

case class AstFunction(
  arguments: List[String],
  statements: List[AstExpression],
  ret: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitFunction(arguments.asJava, statements.asJava, ret)
}

case class AstAssignment(name: String, body: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitAssignment(name, body)
}

case class AstPrint(body: AstExpression) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitPrint(body)
}

case class AstIfZero(
  cond: AstExpression,
  ifTrue: AstExpression,
  ifFalse: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitIf(cond, ifTrue, ifFalse)
}

class EnsoParserInternal extends JavaTokenParsers {

  override def skipWhitespace: Boolean = true

  def delimited[T](beg: String, end: String, parser: Parser[T]): Parser[T] =
    beg ~> parser <~ end

  def nonEmptyList[T](parser: Parser[T]): Parser[List[T]] =
    parser ~ (("," ~> parser) *) ^^ {
      case e ~ es => e :: es
    }

  def long: Parser[AstLong] = wholeNumber ^^ { numStr =>
    AstLong(numStr.toLong)
  }

  def foreign: Parser[AstForeign] =
    ("js" | "rb" | "py") ~ foreignLiteral ^^ {
      case lang ~ code => AstForeign(lang, code)
    }

  def argList: Parser[List[AstExpression]] =
    delimited("[", "]", nonEmptyList(expression))

  def inArgList: Parser[List[String]] = delimited("|", "|", nonEmptyList(ident))

  def foreignLiteral: Parser[String] = "**" ~> "[^\\*]*".r <~ "**"

  def variable: Parser[AstVariable] = ident ^^ AstVariable

  def operand: Parser[AstExpression] =
    long | foreign | variable | "(" ~> expression <~ ")" | call

  def arith: Parser[AstExpression] =
    operand ~ ((("+" | "-" | "*" | "/" | "%") ~ operand) ?) ^^ {
      case a ~ Some(op ~ b) => AstArithOp(op, a, b)
      case a ~ None         => a
    }

  def expression: Parser[AstExpression] = ifZero | arith | function

  def call: Parser[AstApply] = "@" ~> expression ~ (argList ?) ^^ {
    case expr ~ args => AstApply(expr, args.getOrElse(Nil))
  }

  def assignment: Parser[AstAssignment] = ident ~ ("=" ~> expression) ^^ {
    case v ~ exp => AstAssignment(v, exp)
  }

  def print: Parser[AstPrint] = "print:" ~> expression ^^ AstPrint

  def ifZero: Parser[AstIfZero] = "ifZero:" ~> argList ^^ {
    case List(cond, ift, iff) => AstIfZero(cond, ift, iff)
  }

  def function: Parser[AstFunction] =
    ("{" ~> (inArgList ?) ~ ((statement <~ ";") *) ~ expression <~ "}") ^^ {
      case args ~ stmts ~ expr => AstFunction(args.getOrElse(Nil), stmts, expr)
    }

  def statement: Parser[AstExpression] = assignment | print | expression

  def globalScope: Parser[AstGlobalScope] = ((assignment) *) ~ expression ^^ {
    case assignments ~ expr => AstGlobalScope(assignments, expr)
  }

  def parseGlobalScope(code: String): AstGlobalScope = {
    parseAll(globalScope, code).get
  }

  def parse(code: String): AstExpression = {
    parseAll(expression | function, code).get
  }
}

class EnsoParser {

  def parseEnso(code: String): AstGlobalScope = {
    new EnsoParserInternal().parseGlobalScope(code)
  }
}
