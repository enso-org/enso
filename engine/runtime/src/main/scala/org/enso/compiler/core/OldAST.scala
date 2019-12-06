package org.enso.compiler.core

import java.util.Optional

import org.apache.commons.lang3.StringEscapeUtils
import org.enso.syntax.text.Location

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.parsing.combinator._

trait AstExpressionVisitor[+T] {
  def visitLong(l: AstLong): T

  def visitArithOp(astArithOp: AstArithOp): T

  def visitForeign(lang: String, code: String): T

  def visitVariable(astVariable: AstVariable): T

  def visitFunction(function: AstFunction): T

  def visitCaseFunction(function: AstCaseFunction): T

  def visitFunctionApplication(application: AstApply): T

  def visitAssignment(assignment: AstAssignment): T

  def visitMatch(astMatch: AstMatch): T

  def visitForce(target: AstForce): T

  def visitStringLiteral(string: AstStringLiteral): T

  def visitBlock(
    statements: java.util.List[AstExpression],
    retValue: AstExpression,
    suspended: Boolean
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

case class AstLong(location: Option[Location], value: Long)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitLong(this)
}

case class AstStringLiteral(location: Option[Location], string: String)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitStringLiteral(this)
}

case class AstArithOp(
  location: Option[Location],
  op: String,
  left: AstExpression,
  right: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitArithOp(this)
}

case class AstForeign(location: Option[Location], lang: String, code: String)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitForeign(lang, code)
}

case class AstVariable(location: Option[Location], name: String)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitVariable(this)
}

case class AstApply(
  location: Option[Location],
  fun: AstExpression,
  args: List[AstCallArg],
  hasDefaultsSuspended: Boolean
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitFunctionApplication(this)
  def getArgs: java.util.List[AstCallArg] = args.asJava
}

case class AstFunction(
  location: Option[Location],
  arguments: List[AstArgDefinition],
  body: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitFunction(this)

  def getArguments: java.util.List[AstArgDefinition] = arguments.asJava
}

case class AstCaseFunction(
  location: Option[Location],
  arguments: List[AstArgDefinition],
  body: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitCaseFunction(this)

  def getArguments: java.util.List[AstArgDefinition] = arguments.asJava
}

case class AstAssignment(
  location: Option[Location],
  name: String,
  body: AstExpression
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitAssignment(this)
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
    visitor.visitMatch(this)
  def getBranches: java.util.List[AstCase] = branches.asJava
  def getFallback: Optional[AstCaseFunction] =
    Optional.ofNullable(fallback.orNull)
}

case class AstForce(location: Option[Location], target: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitForce(this)
}

case class AstBlock(
  location: Option[Location],
  statements: List[AstExpression],
  retVal: AstExpression,
  suspended: Boolean = false
) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitBlock(statements.asJava, retVal, suspended)
}
