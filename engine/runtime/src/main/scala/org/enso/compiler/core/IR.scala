package org.enso.compiler.core

import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.{AST, Location}
import shapeless.HList

import scala.reflect.ClassTag

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
  */
sealed trait IR {

  /** A list of metadata that the node has been tagged with as the result of
    * various compiler passes.
    */
  val passData: Set[IR.Metadata]

  /** Adds pass metadata to the IR node.
    *
    * @param newData the metadata to add
    * @return the node, with `newData` added to its [[passData]]
    */
  def addMetadata(newData: IR.Metadata): IR

  /** Gets the metadata of the given type from the node, if it exists.
    *
    * @tparam T the type of the metadata to be obtained
    * @return the requested metadata
    */
  def getMetadata[T <: IR.Metadata: ClassTag]: Option[T] = {
    this.passData.collectFirst { case data: T => data }
  }

  /** The source location that the node corresponds to. */
  val location: Option[Location]
}
object IR {

  // === Basic Shapes =========================================================

  /** A node representing an empty IR construct.
    *
    * @param location the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class Empty(
    override val location: Option[Location],
    override val passData: Set[Metadata] = Set()
  ) extends IR
      with IRKind.Primitive {
    override def addMetadata(newData: Metadata): Empty = {
      copy(passData = this.passData + newData)
    }
  }

  /** Allows for the tagging of [[IR]] nodes with arbitrary `data`.
    *
    * The `data` is represented as an [[HList]] to allow for the stacking of
    * multiple pieces of arbitrary data as needed.
    *
    * @param ir the [[IR]] node being tagged
    * @param data the data to associate with `ir`
    * @param location the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    * @tparam T the type of the tag data
    */
  sealed case class Tagged[T <: HList](
    ir: IR,
    data: T,
    override val location: Option[Location],
    override val passData: Set[Metadata] = Set()
  ) extends IR
      with IRKind.Primitive {
    override def addMetadata(newData: Metadata): Tagged[T] = {
      copy(passData = this.passData + newData)
    }
  }

  // === Module ===============================================================

  /** A representation of a top-level Enso module.
    *
    * Modules may only contain imports and top-level bindings, with no top-level
    * executable code.
    *
    * @param imports the import statements that bring other modules into scope
    * @param bindings the top-level bindings for this module
    * @param location the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class Module(
    imports: List[ModuleScope.Import],
    bindings: List[ModuleScope.Definition],
    override val location: Option[Location],
    override val passData: Set[Metadata] = Set()
  ) extends IR
      with IRKind.Primitive {
    override def addMetadata(newData: Metadata): Module = {
      copy(passData = this.passData + newData)
    }
  }

  // === Module Scope =========================================================

  /** A representation of constructs that can only occur in the top-level module
    * scope
    */
  sealed trait ModuleScope extends IR
  object ModuleScope {

    /** An import statement.
      *
      * @param name the full `.`-separated path representing the import
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Import(
      name: String,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends ModuleScope
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Import = {
        copy(passData = this.passData + newData)
      }
    }

    /** A representation of top-level definitions. */
    sealed trait Definition extends ModuleScope
    object Definition {

      /** The definition of an atom constructor and its associated arguments.
        *
        * @param name the name of the atom
        * @param arguments the arguments to the atom constructor
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Atom(
        name: String,
        arguments: List[DefinitionArgument.Specified],
        override val location: Option[Location],
        override val passData: Set[Metadata] = Set()
      ) extends Definition
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Atom = {
          copy(passData = this.passData + newData)
        }
      }

      /** The definition of a method for a given constructor [[typeName]].
        *
        * @param typeName the name of the atom that the method is being defined
        *                 for
        * @param methodName the name of the method being defined on `typename`
        * @param function the body of the method
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Method(
        typeName: String,
        methodName: String,
        function: Function.Lambda,
        override val location: Option[Location],
        override val passData: Set[Metadata] = Set()
      ) extends Definition
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Method = {
          copy(passData = this.passData + newData)
        }
      }
    }
  }

  // === Expression ===========================================================

  /** Enso expressions. */
  sealed trait Expression extends IR
  object Expression {

    // TODO [AA] Remove suspended blocks from Enso.
    /** A block expression.
      *
      * @param expressions the expressions in the block
      * @param returnValue the final expression in the block
      * @param location the source location that the node corresponds to
      * @param suspended whether or not the block is suspended
      * @param passData the pass metadata associated with this node
      */
    sealed case class Block(
      expressions: List[Expression],
      returnValue: Expression,
      override val location: Option[Location],
      suspended: Boolean                   = false,
      override val passData: Set[Metadata] = Set()
    ) extends Expression
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Block = {
        copy(passData = this.passData + newData)
      }
    }

    /** A binding expression of the form `name = expr`
      *
      * @param name the name being bound to
      * @param expression the expression being bound to `name`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    case class Binding(
      name: String,
      expression: Expression,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Expression
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Binding = {
        copy(passData = this.passData + newData)
      }
    }
  }

  // === Literals =============================================================

  /** Enso literals. */
  sealed trait Literal extends Expression with IRKind.Primitive
  object Literal {

    /** A numeric Enso literal.
      *
      * @param value the textual representation of the numeric literal
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Number(
      value: String,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Literal {
      override def addMetadata(newData: Metadata): Number = {
        copy(passData = this.passData + newData)
      }
    }

    /** A textual Enso literal.
      *
      * @param text the text of the literal
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Text(
      text: String,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Literal {
      override def addMetadata(newData: Metadata): Text = {
        copy(passData = this.passData + newData)
      }
    }
  }

  // === Names ================================================================

  /** Enso names. */
  sealed trait Name extends Expression with IRKind.Primitive
  object Name {

    /** The representation of a literal name.
      *
      * @param name the literal text of the name
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Literal(
      name: String,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Name {
      override def addMetadata(newData: Metadata): Name.Literal = {
        copy(passData = this.passData + newData)
      }
    }

    /** A representation of the name `this`, used to refer to the current type.
      *
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class This(
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Name {
      override def addMetadata(newData: Metadata): This = {
        copy(passData = this.passData + newData)
      }
    }

    /** A representation of the name `here`, used to refer to the current
      * module.
      *
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Here(
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Name {
      override def addMetadata(newData: Metadata): Here = {
        copy(passData = this.passData + newData)
      }
    }
  }

  // === Typing ===============================================================

  /** Constructs that operate on types. */
  sealed trait Type extends IR
  object Type {
    // TODO [AA] Type ascription, context ascription, typeset member and the
    //  typeset operators
  }

  // === Function =============================================================

  /** Functions in Enso. */
  sealed trait Function extends Expression {

    /** The function arguments.
      *
      * Please note that while the source language does not represent
      * multi-argument lambdas, the internal language can and does.
      */
    val arguments: List[DefinitionArgument.Specified]

    /** The body of the function */
    val body: Expression

    /** Whether or not the function _can_ be tail-call optimised.
      *
      * Please note that this being set to `true` does not _guarantee_ that the
      * function is optimised.
      */
    val canBeTCO: Boolean
  }
  object Function {

    /** The primitive function type in Enso: `->`.
      *
      * It should be noted that while the _surface_ language does not support
      * multi-argument lambdas, our internal representation does so to allow for
      * better optimisation.
      *
      * @param arguments the arguments to the lambda
      * @param body the body of the lambda
      * @param location the source location that the node corresponds to
      * @param canBeTCO whether or not the function can be tail-call optimised
      * @param passData the pass metadata associated with this node
      */
    case class Lambda(
      override val arguments: List[DefinitionArgument.Specified],
      override val body: Expression,
      override val location: Option[Location],
      override val canBeTCO: Boolean       = true,
      override val passData: Set[Metadata] = Set()
    ) extends Function
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Lambda = {
        copy(passData = this.passData + newData)
      }
    }
  }

  // === Definition-Site Arguments ============================================

  /** Definition-site arguments in Enso. */
  sealed trait DefinitionArgument extends IR
  object DefinitionArgument {

    /** The representation of an argument from a [[Function]] or
      * [[IR.ModuleScope.Definition.Atom]] definition site.
      *
      * @param name the name of the argument
      * @param defaultValue the default value of the argument, if present
      * @param suspended whether or not the argument has its execution suspended
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Specified(
      name: String,
      defaultValue: Option[Expression],
      suspended: Boolean,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends IR
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Specified = {
        copy(passData = this.passData + newData)
      }
    }

    // TODO [AA] Add support for `_` ignored arguments.
  }

  // === Applications =========================================================

  /** All function applications in Enso. */
  sealed trait Application extends Expression
  object Application {

    /** A standard prefix function application.
      *
      * @param function the function being called
      * @param arguments the arguments to the function being called
      * @param hasDefaultsSuspended whether the function application has any
      *                             argument defaults in `function` suspended
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Prefix(
      function: Expression,
      arguments: List[CallArgument],
      hasDefaultsSuspended: Boolean,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Application
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Prefix = {
        copy(passData = this.passData + newData)
      }
    }

    /** A representation of a term that is explicitly forced.
      *
      * @param target the expression being forced
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Force(
      target: Expression,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Application
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Force = {
        copy(passData = this.passData + newData)
      }
    }

    /** Operator applications in Enso. */
    sealed trait Operator extends Application
    object Operator {

      /** A representation of a generic binary operator application in Enso.
        *
        * @param left the left operand to `operator`
        * @param operator the operator function being called
        * @param right the right operand to `operator`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Binary(
        left: Expression,
        operator: String,
        right: Expression,
        override val location: Option[Location],
        override val passData: Set[Metadata] = Set()
      ) extends Application
          with IRKind.Sugar {
        override def addMetadata(newData: Metadata): Binary = {
          copy(passData = this.passData + newData)
        }
      }
    }

    // TODO [AA] Add support for left, right, and centre sections
  }

  // === Call-Site Arguments ==================================================

  /** Call-site arguments in Enso. */
  sealed trait CallArgument extends IR
  object CallArgument {

    /** A representation of an argument at a function call site.
      *
      * @param name the name of the argument being called, if present
      * @param value the expression being passed as the argument's value
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Specified(
      name: Option[String],
      value: Expression,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends CallArgument
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Specified = {
        copy(passData = this.passData + newData)
      }
    }

    // TODO [AA] Add support for the `_` lambda shorthand argument.
  }

  // === Case Expression ======================================================

  /** The Enso case expression. */
  sealed trait Case extends Expression
  object Case {

    /** The main body of the Enso case expression.
      *
      * @param scrutinee the expression whose value is being matched on
      * @param branches the branches of the case expression
      * @param fallback a fallback branch, if provided explicitly
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Expr(
      scrutinee: Expression,
      branches: Seq[Branch],
      fallback: Option[Function],
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Case
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Expr = {
        copy(passData = this.passData + newData)
      }
    }

    /** A branch in a case statement.
      *
      * @param pattern the pattern that attempts to match against the scrutinee
      * @param expression the expression that is executed if the pattern matches
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Branch(
      pattern: Expression,
      expression: Function,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Case
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): IR = {
        copy(passData = this.passData + newData)
      }
    }

    /** The different types of patterns that can occur in a match. */
    trait Pattern extends IR
    object Pattern {
      // TODO [AA] Better differentiate the types of patterns that can occur
    }
  }

  // === Comments =============================================================

  /** Enso comment entities. */
  sealed trait Comment extends Expression {
    val commented: Expression
  }
  object Comment {

    /** A documentation comment in the Enso source.
      *
      * @param commented the expression with which the comment is associated
      * @param doc the documentation of `commented`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Documentation(
      override val commented: Expression,
      doc: Doc,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Comment
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Documentation = {
        copy(passData = this.passData + newData)
      }
    }
  }

  // === Foreign ==============================================================

  /** Foreign code entities. */
  sealed trait Foreign extends Expression
  object Foreign {

    /** A foreign code definition in Enso.
      *
      * @param lang the foreign language being written
      * @param code the code written in `lang`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Definition(
      lang: String,
      code: String,
      override val location: Option[Location],
      override val passData: Set[Metadata] = Set()
    ) extends Expression
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Definition = {
        copy(passData = this.passData + newData)
      }
    }
  }

  // === Errors ===============================================================

  /** A trait for all errors in Enso's IR. */
  sealed trait Error extends Expression
  object Error {

    /** Represents the various kinds of errors in the IR. */
    sealed trait Kind
    object Kind {

      /** Errors that should be reported during the static compilation phase of
        * execution.
        */
      sealed trait Static extends Kind

      /** Errors that should remain at runtime for display during interactive
        * execution.
        */
      sealed trait Interactive extends Kind
    }

    /** A representation of an Enso syntax error.
      *
      * @param ast the erroneous AST
      * @param passData the pass metadata associated with this node
      */
    sealed case class Syntax(
      ast: AST,
      override val passData: Set[Metadata] = Set()
    ) extends Error
        with Kind.Static
        with IRKind.Primitive {
      override val location: Option[Location] = ast.location

      override def addMetadata(newData: Metadata): Syntax = {
        copy(passData = this.passData + newData)
      }
    }

    /** A representation of an invalid piece of IR.
      *
      * @param ir the IR that is invalid
      * @param passData any annotations from compiler passes
      */
    sealed case class InvalidIR(
      ir: IR,
      override val passData: Set[Metadata] = Set()
    ) extends Error
        with Kind.Static
        with IRKind.Primitive {
      override val location: Option[Location] = ir.location

      override def addMetadata(newData: Metadata): InvalidIR = {
        copy(passData = this.passData + newData)
      }
    }
  }

  // ==========================================================================
  // === Primitive / Sugar ====================================================
  // ==========================================================================

  /** A trait representing the classification of IR nodes into either primitive
    * (constructs which will remain after desugaring) or sugar (constructs that
    * should be removed by the desugaring passes).
    */
  sealed trait IRKind
  object IRKind {

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

  // ==========================================================================
  // === Pass Metadata ========================================================
  // ==========================================================================

  /** This trait should be implemented by all metadata elements generated by
    * passes such that it can be stored in each IR node.
    */
  trait Metadata

}
