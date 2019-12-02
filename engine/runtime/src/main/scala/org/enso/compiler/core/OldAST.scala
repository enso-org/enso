package org.enso.compiler.core

import java.util.Optional

import org.apache.commons.lang3.StringEscapeUtils
import org.enso.syntax.text.AST.Location

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.parsing.combinator._

trait AstExpressionVisitor[+T] {
  def visitLong(l: Long): T

  def visitArithOp(op: String, left: AstExpression, right: AstExpression): T

  def visitForeign(lang: String, code: String): T

  def visitVariable(name: String): T

  def visitFunction(
    arguments: java.util.List[AstArgDefinition],
    body: AstExpression
  ): T

  def visitCaseFunction(
    arguments: java.util.List[AstArgDefinition],
    body: AstExpression
  ): T

  def visitFunctionApplication(
    function: AstExpression,
    arguments: java.util.List[AstCallArg],
    defaultsSuspended: Boolean
  ): T

  def visitAssignment(varName: String, expr: AstExpression): T

  def visitMatch(
    target: AstExpression,
    branches: java.util.List[AstCase],
    fallback: java.util.Optional[AstCaseFunction]
  ): T

  def visitDesuspend(target: AstExpression): T

  def visitStringLiteral(string: String): T

  def visitBlock(
    statements: java.util.List[AstExpression],
    retValue: AstExpression
  ): T
}

trait AstModuleScopeVisitor[T] {

  @throws(classOf[Exception])
  def visitModuleScope(
    imports: java.util.List[AstImport],
    typeDefs: java.util.List[AstTypeDef],
    bindings: java.util.List[AstMethodDef],
    expression: java.util.Optional[AstExpression]
  ): java.util.Optional[T]
}

sealed trait AstModuleSymbol

case class AstTypeDef(name: String, arguments: List[AstArgDefinition])
    extends AstModuleSymbol {
  def getArguments: java.util.List[AstArgDefinition] = arguments.asJava
}

case class AstMethodDef(typeName: String, methodName: String, fun: AstFunction)
    extends AstModuleSymbol

case class AstImport(name: String)

case class AstModuleScope(
  imports: List[AstImport],
  bindings: List[AstModuleSymbol],
  expression: Option[AstExpression]
) {

  def visit[T](visitor: AstModuleScopeVisitor[T]): Optional[T] = {
    val types = new java.util.ArrayList[AstTypeDef]()
    val defs  = new java.util.ArrayList[AstMethodDef]()

    bindings.foreach {
      case assignment: AstMethodDef => defs.add(assignment)
      case typeDef: AstTypeDef      => types.add(typeDef)
    }

    visitor.visitModuleScope(
      imports.asJava,
      types,
      defs,
      Optional.ofNullable(expression.orNull)
    )
  }
}

sealed trait AstExpression {
  def location: Option[Location]
  def getLocation: Optional[Location] = Optional.ofNullable(location.orNull)
  def visit[T](visitor: AstExpressionVisitor[T]): T
}

trait AstArgDefinitionVisitor[+T] {

  def visitArg(
    name: String,
    value: Optional[AstExpression],
    suspended: Boolean,
    position: Int
  ): T
}

case class AstArgDefinition(
  name: String,
  defaultValue: Option[AstExpression],
  suspended: Boolean
) {

  def visit[T](visitor: AstArgDefinitionVisitor[T], position: Int): T =
    visitor.visitArg(
      name,
      Optional.ofNullable(defaultValue.orNull),
      suspended,
      position
    )
}

sealed trait AstCallArg {
  def visit[T](visitor: AstCallArgVisitor[T], position: Int): T
}

trait AstCallArgVisitor[+T] {

  def visitCallArg(
    name: Optional[String],
    value: AstExpression,
    position: Int
  ): T
}

case class AstNamedCallArg(name: String, value: AstExpression)
    extends AstCallArg {
  override def visit[T](visitor: AstCallArgVisitor[T], position: Int): T =
    visitor.visitCallArg(Optional.of(name), value, position)
}

case class AstUnnamedCallArg(value: AstExpression) extends AstCallArg {
  override def visit[T](visitor: AstCallArgVisitor[T], position: Int): T =
    visitor.visitCallArg(Optional.empty(), value, position)
}

case class AstLong(location: Option[Location], l: Long) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitLong(l)
}

case class AstStringLiteral(location: Option[Location], string: String)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitStringLiteral(string)
}

case class AstArithOp(
  location: Option[Location],
  op: String,
  left: AstExpression,
  right: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitArithOp(op, left, right)
}

case class AstForeign(location: Option[Location], lang: String, code: String)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitForeign(lang, code)
}

case class AstVariable(location: Option[Location], name: String)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitVariable(name)
}

case class AstApply(
  location: Option[Location],
  fun: AstExpression,
  args: List[AstCallArg],
  hasDefaultsSuspended: Boolean
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitFunctionApplication(fun, getArgs, hasDefaultsSuspended)
  def getArgs: java.util.List[AstCallArg] = args.asJava
}

case class AstFunction(
  location: Option[Location],
  arguments: List[AstArgDefinition],
  body: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitFunction(getArguments, body)

  def getArguments: java.util.List[AstArgDefinition] = arguments.asJava
}

case class AstCaseFunction(
  location: Option[Location],
  arguments: List[AstArgDefinition],
  body: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitCaseFunction(getArguments, body)

  def getArguments: java.util.List[AstArgDefinition] = arguments.asJava
}

case class AstAssignment(
  location: Option[Location],
  name: String,
  body: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitAssignment(name, body)
}

case class AstCase(
  location: Option[Location],
  cons: AstExpression,
  function: AstCaseFunction
)

case class AstMatch(
  location: Option[Location],
  target: AstExpression,
  branches: Seq[AstCase],
  fallback: Option[AstCaseFunction]
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitMatch(
      target,
      branches.asJava,
      Optional.ofNullable(fallback.orNull)
    )
}

case class AstDesuspend(location: Option[Location], target: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitDesuspend(target)
}

case class AstBlock(
  location: Option[Location],
  statements: List[AstExpression],
  retVal: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitBlock(statements.asJava, retVal)
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
    AstLong(None, numStr.toLong)
  }

  def foreign: Parser[AstForeign] =
    ("js" | "rb" | "py") ~ foreignLiteral ^^ {
      case lang ~ code => AstForeign(None, lang, code)
    }

  def argList: Parser[List[AstCallArg]] =
    delimited("[", "]", nonEmptyList(namedCallArg | unnamedCallArg))

  def namedCallArg: Parser[AstNamedCallArg] = ident ~ ("=" ~> expression) ^^ {
    case name ~ expr => AstNamedCallArg(name, expr)
  }

  def unnamedCallArg: Parser[AstCallArg] = expression ^^ AstUnnamedCallArg

  def argDefinition: Parser[AstArgDefinition] =
    ("$" ?) ~ ident ~ (("=" ~> expression) ?) ^^ {
      case susp ~ name ~ value => AstArgDefinition(name, value, susp.isDefined)
    }

  def inArgList: Parser[List[AstArgDefinition]] =
    delimited(
      "|",
      "|",
      nonEmptyList(argDefinition)
    )

  def foreignLiteral: Parser[String] = "**" ~> "[^\\*]*".r <~ "**"

  def string: Parser[AstStringLiteral] = stringLiteral ^^ { lit =>
    AstStringLiteral(
      None,
      StringEscapeUtils.unescapeJava(lit.substring(1, lit.length - 1))
    )
  }

  def variable: Parser[AstVariable] = ident ^^ (AstVariable(None, _))

  def operand: Parser[AstExpression] =
    long | foreign | variable | "(" ~> expression <~ ")" | functionCall

  def arith: Parser[AstExpression] =
    operand ~ ((("+" | "-" | "*" | "/" | "%") ~ operand) ?) ^^ {
      case a ~ Some(op ~ b) => AstArithOp(None, op, a, b)
      case a ~ None         => a
    }

  def expression: Parser[AstExpression] =
    desuspend | matchClause | arith | function | string

  def functionCall: Parser[AstApply] =
    "@" ~> expression ~ (argList ?) ~ defaultSuspend ^^ {
      case expr ~ args ~ hasDefaultsSuspended =>
        AstApply(None, expr, args.getOrElse(Nil), hasDefaultsSuspended)
    }

  def defaultSuspend: Parser[Boolean] =
    ("..." ?) ^^ {
      case Some(_) => true
      case None    => false
    }

  def desuspend: Parser[AstDesuspend] =
    "$" ~> expression ^^ (AstDesuspend(None, _))

  def assignment: Parser[AstAssignment] = ident ~ ("=" ~> expression) ^^ {
    case v ~ exp => AstAssignment(None, v, exp)
  }

  def function: Parser[AstFunction] =
    ("{" ~> (inArgList ?) ~ ((statement <~ ";") *) ~ expression <~ "}") ^^ {
      case args ~ stmts ~ expr =>
        AstFunction(None, args.getOrElse(Nil), AstBlock(None, stmts, expr))
    }

  def caseFunction: Parser[AstCaseFunction] = function ^^ {
    case AstFunction(None, args, AstBlock(None, stmts, ret)) =>
      AstCaseFunction(None, args, AstBlock(None, stmts, ret))
  }

  def caseClause: Parser[AstCase] =
    (expression <~ "~") ~ (caseFunction <~ ";") ^^ {
      case cons ~ fun =>
        AstCase(None, cons, AstCaseFunction(None, fun.arguments, fun.body))
    }

  def matchClause: Parser[AstMatch] =
    ("match" ~> expression <~ "<") ~ (caseClause *) ~ (((caseFunction <~ ";") ?) <~ ">") ^^ {
      case expr ~ cases ~ fallback => AstMatch(None, expr, cases, fallback)
    }

  def statement: Parser[AstExpression] = assignment | expression

  def typeDef: Parser[AstModuleSymbol] =
    "type" ~> ident ~ ((argDefinition | ("(" ~> argDefinition <~ ")")) *) <~ ";" ^^ {
      case name ~ args => AstTypeDef(name, args)
    }

  def methodDef: Parser[AstMethodDef] =
    (ident <~ ".") ~ (ident <~ "=") ~ expression ^^ {
      case typeName ~ methodName ~ body =>
        val fun = body match {
          case b: AstFunction => b
          case _              => AstFunction(None, List(), body)
        }
        AstMethodDef(typeName, methodName, fun)
    }

  def importStmt: Parser[AstImport] =
    "import" ~> ident ~ (("." ~> ident) *) ^^ {
      case seg ~ segs => AstImport((seg :: segs).mkString("."))
    }

  def globalScope: Parser[AstModuleScope] =
    (importStmt *) ~ ((typeDef | methodDef) *) ~ expression ^^ {
      case imports ~ assignments ~ expr =>
        AstModuleScope(imports, assignments, Some(expr))
    }

  def parseGlobalScope(code: String): AstModuleScope = {
    parseAll(globalScope, code).get
  }

  def parse(code: String): AstExpression = {
    parseAll(expression | function, code).get
  }

  def parseLine(code: String): AstExpression = {
    parseAll(statement, code).get
  }
}

class EnsoParser {

  def parseEnso(code: String): AstModuleScope = {
    new EnsoParserInternal().parseGlobalScope(code)
  }

  def parseEnsoInline(code: String): AstExpression = {
    new EnsoParserInternal().parseLine(code)
  }
}
