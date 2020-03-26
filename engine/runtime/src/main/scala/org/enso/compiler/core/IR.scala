package org.enso.compiler.core

import org.enso.compiler.core.IR.{Expression, IdentifiedLocation}
import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.{AST, Debug, Location}

import scala.collection.immutable.{Set => ISet}
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
  val passData: ISet[IR.Metadata]

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
  val location: Option[IdentifiedLocation]

  /** Maps the provided function over any expression defined as a child of the
    * node this is called on.
    *
    * @param fn the function to transform the expressions
    * @return `this`, potentially having had its children transformed by `fn`
    */
  def mapExpressions(fn: Expression => Expression): IR

  /** Pretty prints the IR.
    *
    * @return a pretty-printed representation of the IR
    */
  def pretty: String = Debug.pretty(this.toString)
}
object IR {

  /**
    * Couples a location with a possible source identifier.
    *
    * @param location the code location.
    * @param id the identifier for the location.
    */
  case class IdentifiedLocation(location: Location, id: Option[AST.ID]) {

    /**
      * @return the character index of the start of this source location.
      */
    def start: Int = location.start

    /**
      * @return the character index of the end of this source location.
      */
    def end: Int = location.end

    /**
      * @return the length in characters of this location.
      */
    def length: Int = location.length
  }

  object IdentifiedLocation {

    /**
      * Utility constructor, building a location without an ID.
      *
      * @param location the code location.
      * @return an [[IdentifiedLocation]] corresponding to the input location.
      */
    def apply(location: Location): IdentifiedLocation =
      IdentifiedLocation(location, None)
  }

  // === Basic Shapes =========================================================

  /** A node representing an empty IR construct that can be used in any place.
    *
    * @param location the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class Empty(
    override val location: Option[IdentifiedLocation],
    override val passData: ISet[Metadata] = ISet()
  ) extends IR
      with Expression
      with Error
      with IRKind.Primitive {
    override def addMetadata(newData: Metadata): Empty = {
      copy(passData = this.passData + newData)
    }

    override def mapExpressions(fn: Expression => Expression): Empty = this
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
    imports: List[Module.Scope.Import],
    bindings: List[Module.Scope.Definition],
    override val location: Option[IdentifiedLocation],
    override val passData: ISet[Metadata] = ISet()
  ) extends IR
      with IRKind.Primitive {
    override def addMetadata(newData: Metadata): Module = {
      copy(passData = this.passData + newData)
    }

    override def mapExpressions(fn: Expression => Expression): Module = {
      copy(
        imports  = imports.map(_.mapExpressions(fn)),
        bindings = bindings.map(_.mapExpressions(fn))
      )
    }

    def transformExpressions(
      fn: PartialFunction[Expression, Expression]
    ): Module = {
      copy(
        bindings = bindings.map(_.mapExpressions(_.transformExpressions(fn)))
      )
    }
  }
  object Module {

    /** A representation of constructs that can only occur in the top-level
      * module scope
      */
    sealed trait Scope extends IR {
      override def addMetadata(newData: Metadata): Scope
      override def mapExpressions(fn: Expression => Expression): Scope
    }
    object Scope {

      /** An import statement.
        *
        * @param name the full `.`-separated path representing the import
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Import(
        name: String,
        override val location: Option[IdentifiedLocation],
        override val passData: ISet[Metadata] = ISet()
      ) extends Scope
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Import = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(fn: Expression => Expression): Import = this
      }

      /** A representation of top-level definitions. */
      sealed trait Definition extends Scope {
        override def addMetadata(newData: Metadata): Definition
        override def mapExpressions(fn: Expression => Expression): Definition
      }
      object Definition {

        /** The definition of an atom constructor and its associated arguments.
          *
          * @param name the name of the atom
          * @param arguments the arguments to the atom constructor
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          */
        sealed case class Atom(
          name: IR.Name,
          arguments: List[DefinitionArgument],
          override val location: Option[IdentifiedLocation],
          override val passData: ISet[Metadata] = ISet()
        ) extends Definition
            with IRKind.Primitive {
          override def addMetadata(newData: Metadata): Atom = {
            copy(passData = this.passData + newData)
          }

          override def mapExpressions(fn: Expression => Expression): Atom = {
            copy(
              name      = name.mapExpressions(fn),
              arguments = arguments.map(_.mapExpressions(fn))
            )
          }
        }

        /** The definition of a method for a given constructor [[typeName]].
          *
          * @param typeName the name of the atom that the method is being
          *                 defined for
          * @param methodName the name of the method being defined on `typename`
          * @param body the body of the method
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          */
        // TODO [AA] Separate Method into Method.Binding and Method.Explicit to
        //  account for syntax sugar later.
        sealed case class Method(
          typeName: IR.Name,
          methodName: IR.Name,
          body: Expression,
          override val location: Option[IdentifiedLocation],
          override val passData: ISet[Metadata] = ISet()
        ) extends Definition
            with IRKind.Primitive {
          override def addMetadata(newData: Metadata): Method = {
            copy(passData = this.passData + newData)
          }

          override def mapExpressions(fn: Expression => Expression): Method = {
            copy(
              typeName   = typeName.mapExpressions(fn),
              methodName = methodName.mapExpressions(fn),
              body       = fn(body)
            )
          }
        }
      }
    }
  }

  // === Expression ===========================================================

  /** Enso expressions. */
  sealed trait Expression extends IR {

    /** Performs a recursive traversal of the IR, potentially transforming it.
      *
      * @param fn the function to apply across the IR
      * @return the IR, potentially transformed
      */
    def transformExpressions(
      fn: PartialFunction[Expression, Expression]
    ): Expression = {
      if (fn.isDefinedAt(this)) {
        fn(this)
      } else {
        mapExpressions(_.transformExpressions(fn))
      }
    }

    override def mapExpressions(fn: Expression => Expression): Expression
    override def addMetadata(newData: Metadata): Expression
  }
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
      override val location: Option[IdentifiedLocation],
      suspended: Boolean                    = false,
      override val passData: ISet[Metadata] = ISet()
    ) extends Expression
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Block = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Block = {
        copy(
          expressions = expressions.map(fn),
          returnValue = fn(returnValue)
        )
      }
    }

    /** A binding expression of the form `name = expr`
      *
      * @param name the name being bound to
      * @param expression the expression being bound to `name`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Binding(
      name: IR.Name,
      expression: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Expression
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Binding = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Binding = {
        copy(name = name.mapExpressions(fn), expression = fn(expression))
      }
    }
  }

  // === Literals =============================================================

  /** Enso literals. */
  sealed trait Literal extends Expression with IRKind.Primitive {
    override def mapExpressions(fn: Expression => Expression): Literal
    override def addMetadata(newData: Metadata): Literal
  }
  object Literal {

    /** A numeric Enso literal.
      *
      * @param value the textual representation of the numeric literal
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Number(
      value: String,
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Literal {
      override def addMetadata(newData: Metadata): Number = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Number = this
    }

    /** A textual Enso literal.
      *
      * @param text the text of the literal
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Text(
      text: String,
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Literal {
      override def addMetadata(newData: Metadata): Text = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Text = this
    }
  }

  // === Names ================================================================

  /** Enso names. */
  sealed trait Name extends Expression with IRKind.Primitive {
    val name: String

    override def mapExpressions(fn: Expression => Expression): Name
    override def addMetadata(newData: Metadata): Name
  }
  object Name {

    /** The representation of a literal name.
      *
      * @param name the literal text of the name
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Literal(
      override val name: String,
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Name {
      override def addMetadata(newData: Metadata): Name.Literal = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Literal = this
    }

    /** A representation of the name `this`, used to refer to the current type.
      *
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class This(
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Name {
      override val name: String = "this"

      override def addMetadata(newData: Metadata): This = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): This = this
    }

    /** A representation of the name `here`, used to refer to the current
      * module.
      *
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Here(
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Name {
      override val name: String = "here"

      override def addMetadata(newData: Metadata): Here = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Here = this
    }
  }

  // === Typing ===============================================================

  /** Constructs that operate on types. */
  sealed trait Type extends Expression {
    override def mapExpressions(fn: Expression => Expression): Type
    override def addMetadata(newData: Metadata): Type
  }
  object Type {

    /** Static information about the type operators. */
    sealed trait Info {
      val name: String
    }

    /** The ascription of a type to a value.
      *
      * @param typed the expression being ascribed a type
      * @param signature the signature being ascribed to `typed`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Ascription(
      typed: Expression,
      signature: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Type
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Ascription = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Ascription = {
        copy(typed = fn(typed), signature = fn(signature))
      }
    }
    object Ascription extends Info {
      override val name: String = ":"
    }

    /** A representation of the `in` portion of a type signature that represents
      * the ascription of a monadic context.
      *
      * @param typed the type being ascribed a monadic context
      * @param context the context being ascribed to `typed`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Context(
      typed: Expression,
      context: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Type
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Context = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Context = {
        copy(typed = fn(typed), context = fn(context))
      }
    }
    object Context extends Info {
      override val name: String = "in"
    }

    /** IR nodes for dealing with typesets. */
    sealed trait Set extends Type {
      override def mapExpressions(fn: Expression => Expression): Set
      override def addMetadata(newData: Metadata): Set
    }
    object Set {

      /** The representation of a typeset member.
        *
        * @param label the member's label, if given
        * @param memberType the member's type, if given
        * @param value the member's value, if given
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Member(
        label: Name,
        memberType: Expression,
        value: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: ISet[Metadata] = ISet()
      ) extends Set
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Member = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(fn: Expression => Expression): Member = {
          copy(
            label      = label.mapExpressions(fn),
            memberType = fn(memberType),
            value      = fn(value)
          )
        }
      }
      object Member extends Info {
        override val name: String = "_ : _ = _"
      }

      /** The typeset subsumption judgement `<:`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Subsumption(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: ISet[Metadata] = ISet()
      ) extends Set
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Subsumption = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(
          fn: Expression => Expression
        ): Subsumption = {
          copy(left = fn(left), right = fn(right))
        }
      }
      object Subsumption extends Info {
        override val name: String = "<:"
      }

      /** The typeset equality judgement `~`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Equality(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: ISet[Metadata] = ISet()
      ) extends Set
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Equality = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(fn: Expression => Expression): Equality = {
          copy(left = fn(left), right = fn(right))
        }
      }
      object Equality extends Info {
        override val name: String = "~"
      }

      /** The typeset concatenation operator `,`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Concat(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: ISet[Metadata] = ISet()
      ) extends Set
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Concat = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(fn: Expression => Expression): Concat = {
          copy(left = fn(left), right = fn(right))
        }
      }
      object Concat extends Info {
        override val name: String = ","
      }

      /** The typeset union operator `|`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Union(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: ISet[Metadata] = ISet()
      ) extends Set
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Union = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(fn: Expression => Expression): Union = {
          copy(left = fn(left), right = fn(right))
        }
      }
      object Union extends Info {
        override val name: String = "|"
      }

      /** The typeset intersection operator `&`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Intersection(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: ISet[Metadata] = ISet()
      ) extends Set
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Intersection = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(
          fn: Expression => Expression
        ): Intersection = {
          copy(left = fn(left), right = fn(right))
        }
      }
      object Intersection extends Info {
        override val name: String = "&"
      }

      /** The typeset subtraction operator `\`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        */
      sealed case class Subtraction(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: ISet[Metadata] = ISet()
      ) extends Set
          with IRKind.Primitive {
        override def addMetadata(newData: Metadata): Subtraction = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(
          fn: Expression => Expression
        ): Subtraction = {
          copy(left = fn(left), right = fn(right))
        }
      }
      object Subtraction extends Info {
        override val name: String = "\\"
      }
    }
  }

  // === Function =============================================================

  /** Functions in Enso. */
  sealed trait Function extends Expression {

    /** The function arguments.
      *
      * Please note that while the source language does not represent
      * multi-argument lambdas, the internal language can and does.
      */
    val arguments: List[DefinitionArgument]

    /** The body of the function */
    val body: Expression

    /** Whether or not the function _can_ be tail-call optimised.
      *
      * Please note that this being set to `true` does not _guarantee_ that the
      * function is optimised.
      */
    val canBeTCO: Boolean

    override def mapExpressions(fn: Expression => Expression): Function
    override def addMetadata(newData: Metadata): Function
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
    sealed case class Lambda(
      override val arguments: List[DefinitionArgument],
      override val body: Expression,
      override val location: Option[IdentifiedLocation],
      override val canBeTCO: Boolean        = true,
      override val passData: ISet[Metadata] = ISet()
    ) extends Function
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Lambda = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Lambda = {
        copy(arguments = arguments.map(_.mapExpressions(fn)), body = fn(body))
      }
    }
  }

  // === Definition-Site Arguments ============================================

  /** Definition-site arguments in Enso. */
  sealed trait DefinitionArgument extends IR {
    val defaultValue: Option[Expression]

    override def mapExpressions(
      fn: Expression => Expression
    ): DefinitionArgument

    override def addMetadata(newData: Metadata): DefinitionArgument
  }
  object DefinitionArgument {

    /** The representation of an argument from a [[Function]] or
      * [[IR.Module.Scope.Definition.Atom]] definition site.
      *
      * @param name the name of the argument
      * @param defaultValue the default value of the argument, if present
      * @param suspended whether or not the argument has its execution suspended
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Specified(
      name: IR.Name,
      override val defaultValue: Option[Expression],
      suspended: Boolean,
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends DefinitionArgument
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Specified = {
        copy(passData = this.passData + newData)
      }

      def mapExpressions(fn: Expression => Expression): Specified = {
        copy(
          name         = name.mapExpressions(fn),
          defaultValue = defaultValue.map(fn)
        )
      }
    }

    // TODO [AA] Add support for `_` ignored arguments.
  }

  // === Applications =========================================================

  /** All function applications in Enso. */
  sealed trait Application extends Expression {
    override def mapExpressions(fn: Expression => Expression): Application
    override def addMetadata(newData: Metadata): Application
  }
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
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Application
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Prefix = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Prefix = {
        copy(function = fn(function), arguments.map(_.mapExpressions(fn)))
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
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Application
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Force = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Force = {
        copy(target = fn(target))
      }
    }

    /** Operator applications in Enso. */
    sealed trait Operator extends Application {
      override def mapExpressions(fn: Expression => Expression): Operator
      override def addMetadata(newData: Metadata): Operator
    }
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
        operator: IR.Name,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: ISet[Metadata] = ISet()
      ) extends Operator
          with IRKind.Sugar {
        override def addMetadata(newData: Metadata): Binary = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(fn: Expression => Expression): Binary = {
          copy(left = fn(left), right = fn(right))
        }
      }
    }

    // TODO [AA] Add support for left, right, and centre sections
  }

  // === Call-Site Arguments ==================================================

  /** Call-site arguments in Enso. */
  sealed trait CallArgument extends IR {

    /** The name of the argument, if present. */
    val name: Option[IR.Name]

    override def mapExpressions(fn: Expression => Expression): CallArgument
    override def addMetadata(newData: Metadata): CallArgument
  }
  object CallArgument {

    /** A representation of an argument at a function call site.
      *
      * @param name the name of the argument being called, if present
      * @param value the expression being passed as the argument's value
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      */
    sealed case class Specified(
      override val name: Option[IR.Name],
      value: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends CallArgument
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Specified = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Specified = {
        copy(name = name.map(n => n.mapExpressions(fn)), value = fn(value))
      }
    }

    // TODO [AA] Add support for the `_` lambda shorthand argument (can be
    //  called by name)
  }

  // === Case Expression ======================================================

  /** The Enso case expression. */
  sealed trait Case extends Expression {
    override def mapExpressions(fn: Expression => Expression): Case
    override def addMetadata(newData: Metadata): Case
  }
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
      fallback: Option[Expression],
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Case
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Expr = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Expr = {
        copy(
          scrutinee = fn(scrutinee),
          branches.map(_.mapExpressions(fn)),
          fallback.map(fn)
        )
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
      expression: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Case
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Branch = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Branch = {
        copy(pattern = fn(pattern), expression = fn(expression))
      }
    }

    /** The different types of patterns that can occur in a match. */
    sealed trait Pattern extends IR {
      override def mapExpressions(fn: Expression => Expression): Pattern
      override def addMetadata(newData: Metadata): Pattern
    }
    object Pattern {
      // TODO [AA] Better differentiate the types of patterns that can occur
    }
  }

  // === Comments =============================================================

  /** Enso comment entities. */
  sealed trait Comment extends Expression {
    override def mapExpressions(fn: Expression => Expression): Comment
    override def addMetadata(newData: Metadata): Comment

    /** The expression being commented. */
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
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Comment
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Documentation = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(
        fn: Expression => Expression
      ): Documentation = {
        copy(commented = fn(commented))
      }
    }
  }

  // === Foreign ==============================================================

  /** Foreign code entities. */
  sealed trait Foreign extends Expression {
    override def mapExpressions(fn: Expression => Expression): Foreign
    override def addMetadata(newData: Metadata): Foreign
  }
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
      override val location: Option[IdentifiedLocation],
      override val passData: ISet[Metadata] = ISet()
    ) extends Foreign
        with IRKind.Primitive {
      override def addMetadata(newData: Metadata): Definition = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Definition =
        this
    }
  }

  // === Errors ===============================================================

  /** A trait for all errors in Enso's IR. */
  sealed trait Error extends Expression {
    override def mapExpressions(fn: Expression => Expression): Error
    override def addMetadata(newData: Metadata): Error
  }
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
      override val passData: ISet[Metadata] = ISet()
    ) extends Error
        with Kind.Static
        with IRKind.Primitive {
      override val location: Option[IdentifiedLocation] =
        ast.location.map(IdentifiedLocation(_, ast.id))

      override def addMetadata(newData: Metadata): Syntax = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): Syntax = this
    }

    /** A representation of an invalid piece of IR.
      *
      * @param ir the IR that is invalid
      * @param passData any annotations from compiler passes
      */
    sealed case class InvalidIR(
      ir: IR,
      override val passData: ISet[Metadata] = ISet()
    ) extends Error
        with Kind.Static
        with IRKind.Primitive {
      override val location: Option[IdentifiedLocation] = ir.location

      override def addMetadata(newData: Metadata): InvalidIR = {
        copy(passData = this.passData + newData)
      }

      override def mapExpressions(fn: Expression => Expression): InvalidIR =
        this
    }

    /** Errors pertaining to the redefinition of language constructs that are
      * not allowed to be.
      */
    sealed trait Redefined extends Error
    object Redefined {

      /** An error representing the redefinition of a function argument.
        *
        * @param invalidArgDef the invalid definition
        * @param passData the pass metadata for the error
        */
      sealed case class Argument(
        invalidArgDef: IR.DefinitionArgument,
        override val passData: ISet[Metadata] = ISet()
      ) extends Redefined
          with Kind.Static
          with IRKind.Primitive
          with IR.DefinitionArgument {
        override val defaultValue: Option[Expression] = None
        override val location: Option[IdentifiedLocation] =
          invalidArgDef.location

        override def addMetadata(newData: Metadata): Argument = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(
          fn: Expression => Expression
        ): Argument = this
      }

      /** An error representing the redefinition of a binding in a given scope.
        *
        * While bindings in child scopes are allowed to _shadow_ bindings in
        * parent scopes, a binding cannot be redefined within a given scope.
        *
        * @param invalidBinding the invalid binding
        * @param passData the pass metadata for the error
        */
      sealed case class Binding(
        invalidBinding: IR.Expression.Binding,
        override val passData: ISet[Metadata] = ISet()
      ) extends Redefined
          with Kind.Static
          with IRKind.Primitive {
        override val location: Option[IdentifiedLocation] =
          invalidBinding.location

        override def addMetadata(newData: Metadata): Binding = {
          copy(passData = this.passData + newData)
        }

        override def mapExpressions(fn: Expression => Expression): Binding =
          this
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

    /** This trait encodes that a given piece of [[IR]] is used to represent an
      * optimisation on the IR in Enso.
      */
    sealed trait Optimisation extends IRKind
  }

  // ==========================================================================
  // === Pass Metadata ========================================================
  // ==========================================================================

  /** This trait should be implemented by all metadata elements generated by
    * passes such that it can be stored in each IR node.
    */
  trait Metadata
  object Metadata {

    /** An empty metadata type for passes that do not create any metadata. */
    sealed case class Empty() extends Metadata
  }

}
