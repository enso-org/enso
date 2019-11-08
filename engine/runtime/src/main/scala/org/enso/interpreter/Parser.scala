package org.enso.interpreter

import java.util.Optional

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
    statements: java.util.List[AstExpression],
    retValue: AstExpression
  ): T

  def visitCaseFunction(
    arguments: java.util.List[AstArgDefinition],
    statements: java.util.List[AstExpression],
    retValue: AstExpression
  ): T

  def visitFunctionApplication(
    function: AstExpression,
    arguments: java.util.List[AstCallArg],
    defaultsSuspended: Boolean
  ): T

  def visitIf(
    cond: AstExpression,
    ifTrue: AstExpression,
    ifFalse: AstExpression
  ): T

  def visitAssignment(varName: String, expr: AstExpression): T

  def visitMatch(
    target: AstExpression,
    branches: java.util.List[AstCase],
    fallback: java.util.Optional[AstCaseFunction]
  ): T

  def visitDesuspend(target: AstExpression): T
}

trait AstModuleScopeVisitor[+T] {

  @throws(classOf[Exception])
  def visitModuleScope(
    imports: java.util.List[AstImport],
    typeDefs: java.util.List[AstTypeDef],
    bindings: java.util.List[AstMethodDef],
    expression: AstExpression
  ): T
}

sealed trait AstGlobalSymbol

case class AstTypeDef(name: String, arguments: List[AstArgDefinition])
    extends AstGlobalSymbol {
  def getArguments: java.util.List[AstArgDefinition] = arguments.asJava
}

case class AstMethodDef(typeName: String, methodName: String, fun: AstFunction)
    extends AstGlobalSymbol

case class AstImport(name: String)

case class AstModuleScope(
  imports: List[AstImport],
  bindings: List[AstGlobalSymbol],
  expression: AstExpression
) {

  def visit[T](visitor: AstModuleScopeVisitor[T]): T = {
    val types = new java.util.ArrayList[AstTypeDef]()
    val defs  = new java.util.ArrayList[AstMethodDef]()

    bindings.foreach {
      case assignment: AstMethodDef => defs.add(assignment)
      case typeDef: AstTypeDef      => types.add(typeDef)
    }

    visitor.visitModuleScope(imports.asJava, types, defs, expression)
  }
}

sealed trait AstExpression {
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

case class AstApply(
  fun: AstExpression,
  args: List[AstCallArg],
  hasDefaultsSuspended: Boolean
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitFunctionApplication(fun, args.asJava, hasDefaultsSuspended)
}

case class AstFunction(
  arguments: List[AstArgDefinition],
  statements: List[AstExpression],
  ret: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitFunction(arguments.asJava, statements.asJava, ret)

  def getArguments: java.util.List[AstArgDefinition] = arguments.asJava

  def getStatements: java.util.List[AstExpression] = statements.asJava
}

case class AstCaseFunction(
  arguments: List[AstArgDefinition],
  statements: List[AstExpression],
  ret: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitCaseFunction(arguments.asJava, statements.asJava, ret)
}

case class AstAssignment(name: String, body: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitAssignment(name, body)
}

case class AstIfZero(
  cond: AstExpression,
  ifTrue: AstExpression,
  ifFalse: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitIf(cond, ifTrue, ifFalse)
}

case class AstCase(cons: AstExpression, function: AstCaseFunction)

case class AstMatch(
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

case class AstDesuspend(target: AstExpression) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitDesuspend(target)
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

  def variable: Parser[AstVariable] = ident ^^ AstVariable

  def operand: Parser[AstExpression] =
    long | foreign | variable | "(" ~> expression <~ ")" | functionCall

  def arith: Parser[AstExpression] =
    operand ~ ((("+" | "-" | "*" | "/" | "%") ~ operand) ?) ^^ {
      case a ~ Some(op ~ b) => AstArithOp(op, a, b)
      case a ~ None         => a
    }

  def expression: Parser[AstExpression] =
    desuspend | ifZero | matchClause | arith | function

  def functionCall: Parser[AstApply] =
    "@" ~> expression ~ (argList ?) ~ defaultSuspend ^^ {
      case expr ~ args ~ hasDefaultsSuspended =>
        AstApply(expr, args.getOrElse(Nil), hasDefaultsSuspended)
    }

  def defaultSuspend: Parser[Boolean] =
    ("..." ?) ^^ ({
      case Some(_) => true
      case None    => false
    })

  def desuspend: Parser[AstDesuspend] = "$" ~> expression ^^ AstDesuspend

  def assignment: Parser[AstAssignment] = ident ~ ("=" ~> expression) ^^ {
    case v ~ exp => AstAssignment(v, exp)
  }

  def ifZero: Parser[AstIfZero] =
    "ifZero:" ~> "[" ~> (expression ~ ("," ~> expression ~ ("," ~> expression))) <~ "]" ^^ {
      case cond ~ (ift ~ iff) => AstIfZero(cond, ift, iff)
    }

  def function: Parser[AstFunction] =
    ("{" ~> (inArgList ?) ~ ((statement <~ ";") *) ~ expression <~ "}") ^^ {
      case args ~ stmts ~ expr => AstFunction(args.getOrElse(Nil), stmts, expr)
    }

  def caseFunction: Parser[AstCaseFunction] = function ^^ {
    case AstFunction(args, stmts, ret) => AstCaseFunction(args, stmts, ret)
  }

  def caseClause: Parser[AstCase] =
    (expression <~ "~") ~ (caseFunction <~ ";") ^^ {
      case cons ~ fun =>
        AstCase(cons, AstCaseFunction(fun.arguments, fun.statements, fun.ret))
    }

  def matchClause: Parser[AstMatch] =
    ("match" ~> expression <~ "<") ~ (caseClause *) ~ (((caseFunction <~ ";") ?) <~ ">") ^^ {
      case expr ~ cases ~ fallback => AstMatch(expr, cases, fallback)
    }

  def statement: Parser[AstExpression] = assignment | expression

  def typeDef: Parser[AstGlobalSymbol] =
    "type" ~> ident ~ ((argDefinition | ("(" ~> argDefinition <~ ")")) *) <~ ";" ^^ {
      case name ~ args => AstTypeDef(name, args)
    }

  def methodDef: Parser[AstMethodDef] =
    (ident <~ ".") ~ (ident <~ "=") ~ expression ^^ {
      case typeName ~ methodName ~ body =>
        val thisArg =
          AstArgDefinition(Constants.THIS_ARGUMENT_NAME, None, false)
        val fun = body match {
          case b: AstFunction =>
            b.copy(arguments = thisArg :: b.arguments)
          case _ => AstFunction(List(thisArg), List(), body)
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
        AstModuleScope(imports, assignments, expr)
    }

  def parseGlobalScope(code: String): AstModuleScope = {
    parseAll(globalScope, code).get
  }

  def parse(code: String): AstExpression = {
    parseAll(expression | function, code).get
  }
}

class EnsoParser {

  def parseEnso(code: String): AstModuleScope = {
    new EnsoParserInternal().parseGlobalScope(code)
  }
}
