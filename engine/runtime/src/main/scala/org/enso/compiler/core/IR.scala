package org.enso.compiler.core

import java.util.Optional

import org.enso.compiler.core.IR._
import org.enso.syntax.text.{AST, Location}
import shapeless.HList

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

// TODO [AA] Refactor into a proper nested hierarchy once all clients are
//  written in scala.
// TODO [AA] Add location information to the constructs that are currently
//  missing it but should have it (primarily for tracking during desugaring).
// TODO [AA] Loosen type-based restrictions on what can appear where as
//  necessary.
// TODO [AA] Have more things fall into the expression hierarchy as the visitor
//  goes away.

/** [[IR]] is a temporary and fairly unsophisticated internal representation
  * format for Enso programs.
  *
  * It is a purely tree-based representation to support basic desugaring and
  * analysis passes that do not rely on the ability to create cycles in the IR
  * itself. Its existence is the natural evolution of the older AstExpression
  * format used during the initial development of the interpreter.
  *
  * In time, it will be replaced by [[Core]], but expediency dictates that we
  * retain and evolve this representation for the near future.
  *
  * PLEASE NOTE: None of the visitor functions are documented as they are slated
  * for removal as part of the next task.
  */
sealed trait IR
object IR {

  // === Basic Shapes =========================================================

  /** An IR node representing an empty construct. */
  sealed case class Empty() extends IR with IRKind.Primitive

  /** Allows for the tagging of [[IR]] nodes with arbitrary [[data]].
    *
    * The [[data]] is represented as an [[HList]] to allow for the stacking of
    * multiple pieces of arbitrary data as needed.
    *
    * @param ir the [[IR]] node being tagged
    * @param data the data to associated with [[ir]]
    * @tparam T the type of the arbitrary data
    */
  sealed case class Tagged[T <: HList](ir: IR, data: T)
      extends IR
      with IRKind.Primitive

  // === Literals =============================================================

  /** A trait representing all Enso literals. */
  sealed trait Literal extends Expression with IRKind.Primitive

  /** A numeric Enso literal.
    *
    * @param location the source location of the literal
    * @param value the textual representation of the numeric literal
    */
  sealed case class NumberLiteral(location: Option[Location], value: String)
      extends Literal {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitLong(this)
  }

  /** A textual Enso literal.
    *
    * @param location the source location of the literal
    * @param text the text of the literal
    */
  sealed case class TextLiteral(location: Option[Location], text: String)
      extends Literal {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitStringLiteral(this)
  }

  // === Names ================================================================

  /** A trait representing all kinds of name in Enso. */
  sealed trait Name extends Expression with IRKind.Primitive

  /** The representation of a literal name.
    *
    * @param location the source location of tha name occurrence.
    * @param name the literal text of the name
    */
  sealed case class LiteralName(location: Option[Location], name: String)
      extends Name {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitName(this)
  }

  // TODO [AA] Add `this` and `here` as names.

  // === Module ===============================================================

  // TODO [AA] Need to add a `name` field to the module
  /** A representation of a top-level Enso module.
    *
    * Modules may only contain imports and top-level bindings, with no top-level
    * executable code.
    *
    * @param imports the import statements that bring other modules into scope
    * @param bindings the top-level bindings for this module
    */
  sealed case class Module(
    imports: List[AstImport],
    bindings: List[TopLevelSymbol]
  ) extends IR
      with IRKind.Primitive {

    def visit[T](visitor: AstModuleScopeVisitor[T]): Unit = {
      val types = new java.util.ArrayList[AtomDef]()
      val defs  = new java.util.ArrayList[MethodDef]()

      bindings.foreach {
        case assignment: MethodDef => defs.add(assignment)
        case typeDef: AtomDef      => types.add(typeDef)
      }

      visitor.visitModuleScope(
        imports.asJava,
        types,
        defs
      )
    }
  }

  // === Top-Level Symbols ====================================================

  /** A top-level symbol is one that can occur only at the top-level of an Enso
    * module.
    */
  sealed trait TopLevelSymbol extends IR

  /** The definition of an atom constructor and its associated arguments.
    *
    * @param name the name of the atom
    * @param arguments the arguments to the atom constructor
    */
  sealed case class AtomDef(
    name: String,
    arguments: List[DefinitionSiteArgument]
  ) extends TopLevelSymbol
      with IRKind.Primitive {
    def getArguments: java.util.List[DefinitionSiteArgument] = arguments.asJava
  }

  /** The definition of a method for a given constructor [[typeName]].
    *
    * @param typeName the name of the atom that the method is being defined on
    * @param methodName the name of the method being defined on `typename`
    * @param function the body of the method
    */
  sealed case class MethodDef(
    typeName: String,
    methodName: String,
    function: Lambda
  ) extends TopLevelSymbol
      with IRKind.Primitive

  /** An import statement.
    *
    * @param name the full `.`-separated path representing the import
    */
  sealed case class AstImport(name: String) extends IR with IRKind.Primitive

  // === Expression ===========================================================

  /** A representation of all Enso program expressions. */
  sealed trait Expression extends IR {

    /** The source location of the Expression.
      *
      * @return the expression's source location
      */
    def location: Option[Location]

    /** Gets the location from the expression.
      *
      * @return the location, if it exists, otherwise `null`
      */
    def getLocation: Optional[Location] = Optional.ofNullable(location.orNull)

    def visit[T](visitor: AstExpressionVisitor[T]): T
  }

  // === Typing ===============================================================

  sealed trait Type extends IR

  // TODO [AA] Type ascription, context ascription, typeset member and the
  //  typeset operators

  // === Function =============================================================

  /** A trait to represent all Enso function formats. */
  sealed trait Function extends Expression

  /** The primitive function type in Enso: `->`.
    *
    * It should be noted that while the _surface_ language does not support
    * multi-argument lambdas, our internal representation does so to allow for
    * better optimisation.
    *
    * @param location the source location of the lambda definition
    * @param arguments the arguments to the lambda
    * @param body the body of the lambda
    */
  case class Lambda(
    location: Option[Location],
    arguments: List[DefinitionSiteArgument],
    body: Expression
  ) extends Function
      with IRKind.Primitive {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitLambda(this)

    def getArguments: java.util.List[DefinitionSiteArgument] = arguments.asJava
  }

  // === Definition-Site Arguments ============================================

  /** The representation of an argument from a [[Lambda]] or [[AtomDef]]
    * definition site.
    *
    * @param name the name of the argument
    * @param defaultValue the default value of the argument, if present
    * @param suspended whether or not the argument has its execution suspended
    */
  sealed case class DefinitionSiteArgument(
    name: String,
    defaultValue: Option[Expression],
    suspended: Boolean
  ) extends IR
      with IRKind.Primitive {

    def visit[T](visitor: AstArgDefinitionVisitor[T], position: Int): T =
      visitor.visitArg(
        name,
        Optional.ofNullable(defaultValue.orNull),
        suspended,
        position
      )
  }

  // TODO [AA] Add support for `_` ignored arguments.

  // === Applications =========================================================

  /** A representation of all function applications in Enso. */
  sealed trait Application extends Expression

  /** A standard prefix function application
    *
    * @param location the source location of the application
    * @param function the function being called
    * @param arguments the arguments to the function being called
    * @param hasDefaultsSuspended whether the function application has any
    *                             argument defaults in `function` suspended
    */
  sealed case class Prefix(
    location: Option[Location],
    function: Expression,
    arguments: List[CallArgumentDefinition],
    hasDefaultsSuspended: Boolean
  ) extends Application
      with IRKind.Primitive {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitFunctionApplication(this)
    def getArgs: java.util.List[CallArgumentDefinition] = arguments.asJava
  }

  /** A representation of a generic binary operator in Enso.
    *
    * @param location the source location of the operator
    * @param left the left operand to `operator`
    * @param operator the operator function being called
    * @param right the right operand to `operator`
    */
  sealed case class BinaryOperator(
    location: Option[Location],
    left: Expression,
    operator: String,
    right: Expression
  ) extends Application
      with IRKind.Sugar {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitArithOp(this)
  }

  /** A representation of a term that is explicitly forced.
    *
    * @param location the source location of the force
    * @param target the expression being forced
    */
  sealed case class ForcedTerm(location: Option[Location], target: Expression)
      extends Application
      with IRKind.Primitive {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitForce(this)
  }

  // TODO [AA] Add support for left, right, and centre sections

  // === Call-Site Arguments ==================================================

  /** A representation of an argument at a function call site.
    *
    * @param name the name of the argument being called, if present
    * @param value the expression being passed as the argument's value
    */
  sealed case class CallArgumentDefinition(
    name: Option[String],
    value: Expression
  ) extends IR
      with IRKind.Primitive {
    def visit[T](visitor: AstCallArgVisitor[T], position: Int): T =
      visitor.visitCallArg(name.toJava, value, position)
  }

  // TODO [AA] Add support for the `_` lambda shorthand argument.

  // === Structure ============================================================

  // TODO [AA] Remove suspended blocks from Enso.
  /** A block expression.
    *
    * @param location the source location of the block
    * @param expressions the expressions in the block
    * @param returnValue the final expression in the block
    * @param suspended whether or not the block is suspended
    */
  sealed case class Block(
    location: Option[Location],
    expressions: List[Expression],
    returnValue: Expression,
    suspended: Boolean = false
  ) extends Expression
      with IRKind.Primitive {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitBlock(expressions.asJava, returnValue, suspended)
  }

  /** A binding expression of the form `name = expr`
    *
    * @param location the source location of the binding
    * @param name the name being bound to
    * @param expression the expression being bound to `name`
    */
  case class Binding(
    location: Option[Location],
    name: String,
    expression: Expression
  ) extends Expression
      with IRKind.Primitive {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitAssignment(this)
  }

  // === Case Expression ======================================================

  /** The Enso case expression.
    *
    * @param location the source location of the case expression
    * @param scrutinee the expression whose value is being matched on
    * @param branches the branches of the case expression
    * @param fallback a fallback branch, if provided explicitly
    */
  sealed case class CaseExpr(
    location: Option[Location],
    scrutinee: Expression,
    branches: Seq[CaseBranch],
    fallback: Option[CaseFunction]
  ) extends Expression
      with IRKind.Primitive {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitMatch(this)
    def getBranches: java.util.List[CaseBranch] = branches.asJava
    def getFallback: Optional[CaseFunction] =
      Optional.ofNullable(fallback.orNull)
  }

  // TODO [AA] Should become an expression
  /** A branch in a case statement.
    *
    * @param location the source location of the case branch
    * @param pattern the pattern that attempts to match against the scrutinee
    * @param expression the expression that is executed if the pattern matches
    */
  sealed case class CaseBranch(
    location: Option[Location],
    pattern: Expression,
    expression: CaseFunction
  ) extends IRKind.Primitive

  // TODO [AA] Get rid of case function as ExpressionFactory is moved across.
  /** A representation of the expression body of a case branch.
    *
    * This function type is temporary to aid the visitor and should be removed
    * as part of the next task.
    *
    * @param location the source location of the case function
    * @param arguments the arguments to the function
    * @param body the body of the function
    */
  case class CaseFunction(
    location: Option[Location],
    arguments: List[DefinitionSiteArgument],
    body: Expression
  ) extends Expression
      with IRKind.Primitive {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitCaseFunction(this)

    def getArguments: java.util.List[DefinitionSiteArgument] = arguments.asJava
  }

  // TODO [AA] Better differentiate the types of patterns that can occur

  // === Comments =============================================================

  /** A documentation comment in the Enso source.
    *
    * @param location the source location of the comment
    * @param commented the expression with which the comment is associated
    * @param doc the documentation of `commented`
    */
  sealed case class DocComment(
    location: Option[Location],
    commented: Expression,
    doc: String
  ) extends IRKind.Primitive

  // TODO [AA] The above needs to extend `Expression` once the visitor is gone.

  // === Foreign ==============================================================

  /** A foreign code definition in Enso.
    *
    * @param location the source location of the foreign code definition
    * @param lang the foreign language being written
    * @param code the code written in `lang`
    */
  sealed case class ForeignDefinition(
    location: Option[Location],
    lang: String,
    code: String
  ) extends Expression
      with IRKind.Primitive {
    override def visit[T](visitor: AstExpressionVisitor[T]): T =
      visitor.visitForeign(lang, code)
  }

  // === Errors ===============================================================

  /** A trait for all errors in Enso's IR. */
  sealed trait Error extends IR with IRKind.Primitive
  object Error {

    /** A representation of an Enso syntax error.
      *
      * @param ast the erroneous AST
      */
    sealed case class Syntax(ast: AST) extends Error
  }

  // ==========================================================================
  // === Primitive / Sugar ====================================================
  // ==========================================================================

  /** A trait representing the classification of IR nodes into either primitive
    * (constructs which will remain after desugaring) or sugar (constructs that
    * should be removed by the desugaring passes).
    */
  sealed trait IRKind {}
  object IRKind       {

    /** This trait encodes that a given piece of the [[IR]] is considered to be
      * a primitive construct in Enso.
      */
    sealed trait Primitive extends IRKind

    /** This trait encodes that a given piece of the [[IR]] is considered to
      * represent syntax sugar in Enso.
      *
      * All [[Sugar]] constructs should be desugared into [[Primitive]]
      * constructs as soon as possible.
      */
    sealed trait Sugar extends IRKind
  }
}

// ============================================================================
// === Visitors ===============================================================
// ============================================================================

/** The visitor pattern for the [[Expression]] types.
  *
  * @tparam T the type resultant from the visitor
  */
trait AstExpressionVisitor[+T] {
  def visitLong(l: NumberLiteral): T

  def visitArithOp(astArithOp: BinaryOperator): T

  def visitForeign(lang: String, code: String): T

  def visitName(astName: LiteralName): T

  def visitLambda(function: Lambda): T

  def visitCaseFunction(function: CaseFunction): T

  def visitFunctionApplication(application: Prefix): T

  def visitAssignment(assignment: Binding): T

  def visitMatch(astMatch: CaseExpr): T

  def visitForce(target: ForcedTerm): T

  def visitStringLiteral(string: TextLiteral): T

  def visitBlock(
    statements: java.util.List[Expression],
    retValue: Expression,
    suspended: Boolean
  ): T
}

/** The visitor pattern for the [[AstModuleScope]] types.
  *
  * @tparam T the type resultant from the visitor
  */
trait AstModuleScopeVisitor[T] {

  @throws(classOf[Exception])
  def visitModuleScope(
    imports: java.util.List[AstImport],
    typeDefs: java.util.List[AtomDef],
    bindings: java.util.List[MethodDef]
  ): Unit
}

/** The visitor pattern for the [[DefinitionSiteArgument]] types.
  *
  * @tparam T the type resultant from the visitor
  */
trait AstArgDefinitionVisitor[+T] {

  def visitArg(
    name: String,
    value: Optional[Expression],
    suspended: Boolean,
    position: Int
  ): T
}

/** The visitor pattern for the [[CallArgumentDefinition]] types.
  *
  * @tparam T the type resultant from the visitor
  */
trait AstCallArgVisitor[+T] {

  def visitCallArg(
    name: Optional[String],
    value: Expression,
    position: Int
  ): T
}
