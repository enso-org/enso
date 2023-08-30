package org.enso.compiler.core

import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.ir.{
  Diagnostic,
  DiagnosticStorage,
  Expression,
  IRKind,
  IdentifiedLocation,
  MetadataStorage,
  Name,
  Pattern,
  ProcessingPass
}
import org.enso.syntax.text.Debug

import java.util.UUID

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
  * Please note that all extensions of [[IR]] must reimplement `copy` to keep
  * the id intact when copying nodes. The copy implementation should provide a
  * way to set the id for the copy, but should default to being copied. Care
  * must be taken to not end up with two nodes with the same ID. When using
  * `copy` to duplicate nodes, please ensure that a new ID is provided.
  *
  * See also: Note [IR Equality and hashing]
  */
trait IR extends Serializable {

  /** Storage for metadata that the node has been tagged with as the result of
    * various compiler passes.
    */
  val passData: MetadataStorage

  /** The source location that the node corresponds to. */
  val location: Option[IdentifiedLocation]

  /** Sets the location for an IR node.
    *
    * @param location the new location for the IR node
    * @return the IR node with its location set to `location`
    */
  def setLocation(location: Option[IdentifiedLocation]): IR

  /** Gets the external identifier from an IR node, if it is present.
    *
    * @return the external identifier for this IR node
    */
  def getExternalId: Option[IR.ExternalId] = {
    location.flatMap(l => l.id)
  }

  /** Maps the provided function over any expression defined as a child of the
    * node this is called on.
    *
    * @param fn the function to transform the expressions
    * @return `this`, potentially having had its children transformed by `fn`
    */
  def mapExpressions(fn: Expression => Expression): IR

  /** Gets the list of all children IR nodes of this node.
    *
    * @return this node's children.
    */
  def children: List[IR]

  /** Lists all the nodes in the preorder walk of the tree of this node.
    *
    * @return all the descendants of this node.
    */
  def preorder: List[IR] = this :: children.flatMap(_.preorder)

  /** Pretty prints the IR.
    *
    * @return a pretty-printed representation of the IR
    */
  def pretty: String = Debug.pretty(this.toString)

  /** Gets the node's identifier.
    *
    * @return the node's identifier
    */
  def getId: IR.Identifier = id

  /** A unique identifier for a piece of IR. */
  protected var id: IR.Identifier

  /** Storage for compiler diagnostics related to the IR node. */
  val diagnostics: DiagnosticStorage

  /** Creates a deep structural copy of `this`, representing the same structure.
    *
    * You can choose to keep the location, metadata and diagnostic information
    * in the duplicated copy, as well as whether or not you want to generate new
    * node identifiers or not.
    *
    * @param keepLocations whether or not locations should be kept in the
    *                      duplicated IR
    * @param keepMetadata whether or not the pass metadata should be kept in the
    *                      duplicated IR
    * @param keepDiagnostics whether or not the diagnostics should be kept in
    *                        the duplicated IR
    * @param keepIdentifiers whether or not the identifiers should be
    *                        regenerated in the duplicated IR
    * @return a deep structural copy of `this`
    */
  def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): IR

  /** Shows the IR as code.
    *
    * @param indent the current indentation level
    * @return a string representation of `this`
    */
  def showCode(indent: Int = 0): String
}

/* Note [IR Equality and hashing]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * As the IRs are implemented as case classes, their equality is determined by
 * the values included in the constructor. These include the MetadataStorage and
 * DiagnosticStorage. These two storages break the contract of `hashCode` by
 * overriding the `equals` method to compare for equality by their contents, but
 * not `hashCode` (because it would have to be mutable). As the case classes of
 * the IR use that to implement their own equality and hashing, their
 * implementation is also troubled by this. Instances of IR that are equal by
 * the `equals` function, may still return different `hashCode`.
 *
 * The MetadataStorage and DiagnosticStorage should not be used for checking
 * equality of the IR. This should be addressed when the IR is refactored to be
 * properly mutable.
 */

object IR {

  /** Creates a random identifier.
    *
    * @return a random identifier
    */
  def randomId: IR.Identifier = {
    UUID.randomUUID()
  }

  /** The type of identifiers for IR nodes. */
  type Identifier = UUID

  /** The type of external identifiers */
  type ExternalId = UUID

  /** Generates an indent of `n` spaces.
    *
    * @param n the number of spaces
    * @return a string representing an `n`-space indent
    */
  def mkIndent(n: Int): String = {
    " " * n
  }

  /** The size of a single indentation level. */
  val indentLevel: Int = 4

  // === Typing ===============================================================

  /** Constructs that operate on types. */
  sealed trait Type extends Expression {

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Type

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Type

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Type
  }
  object Type {

    /** Static information about the type operators. */
    sealed trait Info {
      val name: String
    }

    sealed case class Function(
      args: List[Expression],
      result: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Type {
      override protected var id: Identifier = randomId

      def copy(
        args: List[Expression]               = args,
        result: Expression                   = result,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Function = {
        val res = Function(args, result, location, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Function =
        copy(
          args = args.map(
            _.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            )
          ),
          result = result.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Function = copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Function = {
        copy(args = args.map(fn), result = fn(result))
      }

      /** @inheritdoc */
      override def toString: String =
        s"""IR.Type.Function(
           |args = $args,
           |result = $result,
           |location = $location,
           |passData = ${this.showPassData},
           |diagnostics = $diagnostics,
           |id = $id
           |)
           |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = args :+ result

      /** @inheritdoc */
      override def showCode(indent: Int): String =
        s"${args.map(_.showCode()).mkString(" -> ")} -> ${result.showCode()}"
    }

    /** The ascription of a type to a value.
      *
      * @param typed the expression being ascribed a type
      * @param signature the signature being ascribed to `typed`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Ascription(
      typed: Expression,
      signature: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Type
        with ir.module.scope.Definition
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param typed the expression being ascribed a type
        * @param signature the signature being ascribed to `typed`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        typed: Expression                    = typed,
        signature: Expression                = signature,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Ascription = {
        val res = Ascription(typed, signature, location, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Ascription =
        copy(
          typed = typed.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          signature = signature.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Ascription = copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Ascription = {
        copy(typed = fn(typed), signature = fn(signature))
      }

      /** @inheritdoc */
      override def toString: String =
        s"""IR.Type.Ascription(
        |typed = $typed,
        |signature = $signature,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = List(typed, signature)

      /** @inheritdoc */
      override def showCode(indent: Int): String =
        s"${typed.showCode(indent)} : ${signature.showCode(indent)}"
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
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Context(
      typed: Expression,
      context: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Type
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates ac opy of `this`.
        *
        * @param typed the type being ascribed a monadic context
        * @param context the context being ascribed to `typed`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        typed: Expression                    = typed,
        context: Expression                  = context,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Context = {
        val res = Context(typed, context, location, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Context =
        copy(
          typed = typed.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          context = context.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Context =
        copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Context = {
        copy(typed = fn(typed), context = fn(context))
      }

      /** @inheritdoc */
      override def toString: String =
        s"""IR.Type.Context(
        |typed = $typed,
        |context = $context,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = List(typed, context)

      /** @inheritdoc */
      override def showCode(indent: Int): String =
        s"${typed.showCode(indent)} in ${context.showCode(indent)}"
    }
    object Context extends Info {
      override val name: String = "in"
    }

    /** Represents the ascription of an error context to an expression.
      *
      * @param typed the expression being ascribed an error context
      * @param error the error being ascribed
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Error(
      typed: Expression,
      error: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Type
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param typed the expression being ascribed an error context
        * @param error the error being ascribed
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        typed: Expression                    = typed,
        error: Expression                    = error,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Error = {
        val res = Error(typed, error, location, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Error =
        copy(
          typed = typed.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepLocations
          ),
          error = error.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepLocations
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Error =
        copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Error =
        copy(typed = fn(typed), error = fn(error))

      /** @inheritdoc */
      override def toString: String =
        s"""IR.Type.Error(
        |typed = $typed,
        |error = $error,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = List(typed, error)

      /** @inheritdoc */
      override def showCode(indent: Int): String =
        s"(${typed.showCode(indent)} ! ${error.showCode(indent)})"
    }
    object Error extends Info {
      override val name: String = "!"
    }

    /** IR nodes for dealing with typesets. */
    sealed trait Set extends Type {

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Set

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Set

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Set
    }
    object Set {

      /** The representation of a typeset member.
        *
        * @param label the member's label, if given
        * @param memberType the member's type, if given
        * @param value the member's value, if given
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Member(
        label: Name,
        memberType: Expression,
        value: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param label the member's label, if given
          * @param memberType the member's type, if given
          * @param value the member's value, if given
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          label: Name                          = label,
          memberType: Expression               = memberType,
          value: Expression                    = value,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Member = {
          val res =
            Member(label, memberType, value, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Member =
          copy(
            label = label.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            memberType = memberType
              .duplicate(
                keepLocations,
                keepMetadata,
                keepDiagnostics,
                keepIdentifiers
              ),
            value = value.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = if (keepIdentifiers) id else randomId
          )

        /** @inheritdoc */
        override def setLocation(location: Option[IdentifiedLocation]): Member =
          copy(location = location)

        /** @inheritdoc */
        override def mapExpressions(fn: Expression => Expression): Member = {
          copy(
            label      = label.mapExpressions(fn),
            memberType = fn(memberType),
            value      = fn(value)
          )
        }

        /** @inheritdoc */
        override def toString: String =
          s"""
          |IR.Type.Set.Member(
          |label = $label,
          |memberType = $memberType,
          |value = $value,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |)
          |""".toSingleLine

        /** @inheritdoc */
        override def children: List[IR] = List(label, memberType, value)

        /** @inheritdoc */
        override def showCode(indent: Int): String = {
          val typeString  = s" : ${memberType.showCode(indent)}"
          val valueString = s" = ${value.showCode(indent)}"
          s"(${label.showCode(indent)}$typeString$valueString)"
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
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Subsumption(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Subsumption = {
          val res = Subsumption(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Subsumption =
          copy(
            left = left.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            right = right.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = if (keepIdentifiers) id else randomId
          )

        /** @inheritdoc */
        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Subsumption = copy(location = location)

        /** @inheritdoc */
        override def mapExpressions(
          fn: Expression => Expression
        ): Subsumption = {
          copy(left = fn(left), right = fn(right))
        }

        /** @inheritdoc */
        override def toString: String =
          s"""
          |IR.Type.Set.Subsumption(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        /** @inheritdoc */
        override def children: List[IR] = List(left, right)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)} <: ${right.showCode(indent)})"
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
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Equality(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Equality = {
          val res = Equality(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Equality =
          copy(
            left = left.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            right = right.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = if (keepIdentifiers) id else randomId
          )

        /** @inheritdoc */
        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Equality = copy(location = location)

        /** @inheritdoc */
        override def mapExpressions(fn: Expression => Expression): Equality = {
          copy(left = fn(left), right = fn(right))
        }

        /** @inheritdoc */
        override def toString: String =
          s"""
          |IR.Type.Set.Equality(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        /** @inheritdoc */
        override def children: List[IR] = List(left, right)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)} ~ ${right.showCode(indent)}"
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
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Concat(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Concat = {
          val res = Concat(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Concat =
          copy(
            left = left.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            right = right.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = if (keepIdentifiers) id else randomId
          )

        /** @inheritdoc */
        override def setLocation(location: Option[IdentifiedLocation]): Concat =
          copy(location = location)

        /** @inheritdoc */
        override def mapExpressions(fn: Expression => Expression): Concat = {
          copy(left = fn(left), right = fn(right))
        }

        /** @inheritdoc */
        override def toString: String =
          s"""
          |IR.Type.Set.Concat(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        /** @inheritdoc */
        override def children: List[IR] = List(left, right)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)}; ${right.showCode(indent)})"
      }
      object Concat extends Info {
        override val name: String = ";"
      }

      /** The typeset union operator `|`.
        *
        * @param operands the operands
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Union(
        operands: List[Expression],
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          operands: List[Expression]           = operands,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Union = {
          val res = Union(operands, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Union =
          copy(
            operands = operands.map(
              _.duplicate(
                keepLocations,
                keepMetadata,
                keepDiagnostics,
                keepIdentifiers
              )
            ),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = if (keepIdentifiers) id else randomId
          )

        /** @inheritdoc */
        override def setLocation(location: Option[IdentifiedLocation]): Union =
          copy(location = location)

        /** @inheritdoc */
        override def mapExpressions(fn: Expression => Expression): Union = {
          copy(operands = operands.map(fn))
        }

        /** @inheritdoc */
        override def toString: String =
          s"""
          |IR.Type.Set.Union(
          |operands = $operands,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        /** @inheritdoc */
        override def children: List[IR] = operands.toList

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          operands.map(_.showCode(indent)).toList.mkString(" | ")
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
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Intersection(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Intersection = {
          val res = Intersection(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Intersection =
          copy(
            left = left.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            right = right.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = if (keepIdentifiers) id else randomId
          )

        /** @inheritdoc */
        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Intersection = copy(location = location)

        /** @inheritdoc */
        override def mapExpressions(
          fn: Expression => Expression
        ): Intersection = {
          copy(left = fn(left), right = fn(right))
        }

        /** @inheritdoc */
        override def toString: String =
          s"""
          |IR.Type.Set.Intersection(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        /** @inheritdoc */
        override def children: List[IR] = List(left, right)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)} & ${right.showCode(indent)})"
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
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Subtraction(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Subtraction = {
          val res = Subtraction(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Subtraction =
          copy(
            left = left.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            right = right.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = if (keepIdentifiers) id else randomId
          )

        /** @inheritdoc */
        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Subtraction = copy(location = location)

        /** @inheritdoc */
        override def mapExpressions(
          fn: Expression => Expression
        ): Subtraction = {
          copy(left = fn(left), right = fn(right))
        }

        /** @inheritdoc */
        override def toString: String =
          s"""
          |IR.Type.Set.Subtraction(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        /** @inheritdoc */
        override def children: List[IR] = List(left, right)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)} \\ ${right.showCode(indent)})"
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

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Function

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Function

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Function
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
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Lambda(
      override val arguments: List[DefinitionArgument],
      override val body: Expression,
      override val location: Option[IdentifiedLocation],
      override val canBeTCO: Boolean              = true,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Function
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param arguments the arguments to the lambda
        * @param body the body of the lambda
        * @param location the source location that the node corresponds to
        * @param canBeTCO whether or not the function can be tail-call optimised
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        arguments: List[DefinitionArgument]  = arguments,
        body: Expression                     = body,
        location: Option[IdentifiedLocation] = location,
        canBeTCO: Boolean                    = canBeTCO,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Lambda = {
        val res =
          Lambda(arguments, body, location, canBeTCO, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Lambda =
        copy(
          arguments = arguments.map(
            _.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            )
          ),
          body = body.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Lambda =
        copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Lambda = {
        copy(arguments = arguments.map(_.mapExpressions(fn)), body = fn(body))
      }

      /** @inheritdoc */
      override def toString: String =
        s"""
        |IR.Function.Lambda(
        |arguments = $arguments,
        |body = $body,
        |location = $location,
        |canBeTCO = $canBeTCO,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = arguments :+ body

      /** @inheritdoc */
      override def showCode(indent: Int): String = {
        val args = arguments.map(_.showCode(indent)).mkString(" ")
        val bodyStr = if (body.isInstanceOf[ir.Expression.Block]) {
          s"\n${body.showCode(indent)}"
        } else {
          s"${body.showCode(indent)}"
        }

        s"$args -> $bodyStr"
      }
    }

    /** A representation of the syntactic sugar for defining functions.
      *
      * @param name the name of the function
      * @param arguments the arguments to the function
      * @param body the body of the function
      * @param location the source location that the node corresponds to
      * @param canBeTCO whether or not the function can be tail-call optimised
      * @param passData the pass metadata associated with this node
      * @param diagnostics the compiler diagnostics for this node
      */
    sealed case class Binding(
      name: Name,
      override val arguments: List[DefinitionArgument],
      override val body: Expression,
      override val location: Option[IdentifiedLocation],
      override val canBeTCO: Boolean              = true,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Function
        with IRKind.Sugar {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the name of the function
        * @param arguments the arguments to the function
        * @param body the body of the function
        * @param location the source location that the node corresponds to
        * @param canBeTCO whether or not the function can be tail-call optimised
        * @param passData the pass metadata associated with this node
        * @param diagnostics the compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        name: Name                           = name,
        arguments: List[DefinitionArgument]  = arguments,
        body: Expression                     = body,
        location: Option[IdentifiedLocation] = location,
        canBeTCO: Boolean                    = canBeTCO,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Binding = {
        val res =
          Binding(
            name,
            arguments,
            body,
            location,
            canBeTCO,
            passData,
            diagnostics
          )
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Binding =
        copy(
          name = name.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          arguments = arguments.map(
            _.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            )
          ),
          body = body.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Binding =
        copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Binding =
        copy(
          name      = name.mapExpressions(fn),
          arguments = arguments.map(_.mapExpressions(fn)),
          body      = fn(body)
        )

      /** @inheritdoc */
      override def toString: String =
        s"""
        |IR.Function.Binding(
        |name = $name,
        |arguments = $arguments,
        |body = $body,
        |location = $location,
        |canBeTCO = $canBeTCO,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = (name :: arguments) :+ body

      /** @inheritdoc */
      override def showCode(indent: Int): String = {
        val argsStr = arguments.map(_.showCode(indent)).mkString(" ")
        val bodyStr = if (body.isInstanceOf[ir.Expression.Block]) {
          s"\n${body.showCode(indent)}"
        } else {
          s"${body.showCode(indent)}"
        }

        s"${name.name} $argsStr = $bodyStr"
      }
    }
  }

  // === Definition-Site Arguments ============================================

  /** Definition-site arguments in Enso. */
  sealed trait DefinitionArgument extends IR {

    /** The name of the argument. */
    val name: Name

    /** The type of the argument */
    val ascribedType: Option[Expression]

    /** The default value of the argument. */
    val defaultValue: Option[Expression]

    /** Whether or not the argument is suspended. */
    val suspended: Boolean

    /** @inheritdoc */
    override def mapExpressions(
      fn: Expression => Expression
    ): DefinitionArgument

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): DefinitionArgument

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): DefinitionArgument

    def withName(ir: Name): DefinitionArgument
  }
  object DefinitionArgument {

    /** The representation of an argument from a [[Function]] or
      * [[ir.module.scope.Definition.Data]] definition site.
      *
      * To create an ignored argument, the argument name should be an
      * [[Name.Blank]].
      *
      * @param name the name of the argument
      * @param ascribedType the explicitly ascribed type of the argument, if
      *                     present
      * @param defaultValue the default value of the argument, if present
      * @param suspended whether or not the argument has its execution suspended
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Specified(
      override val name: Name,
      override val ascribedType: Option[Expression],
      override val defaultValue: Option[Expression],
      override val suspended: Boolean,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends DefinitionArgument
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the name of the argument
        * @param ascribedType the explicitly ascribed type of the argument, if
        *                     present
        * @param defaultValue the default value of the argument, if present
        * @param suspended whether or not the argument has its execution suspended
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        name: Name                           = name,
        ascribedType: Option[Expression]     = ascribedType,
        defaultValue: Option[Expression]     = defaultValue,
        suspended: Boolean                   = suspended,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Specified = {
        val res = Specified(
          name,
          ascribedType,
          defaultValue,
          suspended,
          location,
          passData,
          diagnostics
        )
        res.id = id
        res
      }

      override def withName(ir: Name): DefinitionArgument = copy(name = ir)

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Specified =
        copy(
          name = name.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          ascribedType = ascribedType.map(
            _.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            )
          ),
          defaultValue = defaultValue.map(
            _.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            )
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Specified = copy(location = location)

      /** @inheritdoc */
      def mapExpressions(fn: Expression => Expression): Specified = {
        copy(
          name         = name.mapExpressions(fn),
          ascribedType = ascribedType.map(fn),
          defaultValue = defaultValue.map(fn)
        )
      }

      /** @inheritdoc */
      override def toString: String =
        s"""
        |IR.DefinitionArgument.Specified(
        |name = $name,
        |ascribedType = $ascribedType,
        |defaultValue = $defaultValue,
        |suspended = $suspended,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] =
        name :: ascribedType.toList ++ defaultValue.toList

      /** @inheritdoc */
      override def showCode(indent: Int): String = {
        val withoutLazy =
          if (defaultValue.isDefined && ascribedType.isDefined) {
            val name        = this.name.showCode(indent)
            val typeExpr    = this.ascribedType.get.showCode(indent)
            val defaultExpr = this.defaultValue.get.showCode(indent)
            s"($name : ($typeExpr) = ($defaultExpr))"
          } else if (defaultValue.isDefined) {
            val name        = this.name.showCode(indent)
            val defaultExpr = this.defaultValue.get.showCode(indent)
            s"($name = $defaultExpr)"
          } else if (ascribedType.isDefined) {
            val name     = this.name.showCode(indent)
            val typeExpr = this.ascribedType.get.showCode(indent)
            s"($name : $typeExpr)"
          } else {
            s"${name.showCode(indent)}"
          }

        if (suspended) {
          s"~$withoutLazy"
        } else {
          withoutLazy
        }
      }
    }
  }

  // === Call-Site Arguments ==================================================

  /** Call-site arguments in Enso. */
  sealed trait CallArgument extends IR {

    /** The name of the argument, if present. */
    val name: Option[Name]

    /** The expression of the argument, if present. */
    val value: Expression

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): CallArgument

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): CallArgument

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): CallArgument
  }
  object CallArgument {

    /** A representation of an argument at a function call site.
      *
      * A [[CallArgument]] where the `value` is an [[Name.Blank]] is a
      * representation of a lambda shorthand argument.
      *
      * @param name        the name of the argument being called, if present
      * @param value       the expression being passed as the argument's value
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Specified(
      override val name: Option[Name],
      override val value: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends CallArgument
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the name of the argument being called, if present
        * @param value the expression being passed as the argument's value
        * @param location the source location that the node corresponds to
        * @param shouldBeSuspended whether or not the argument should be passed
        *        suspended
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        name: Option[Name]                   = name,
        value: Expression                    = value,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Specified = {
        val res = Specified(
          name,
          value,
          location,
          passData,
          diagnostics
        )
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Specified =
        copy(
          name = name.map(
            _.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            )
          ),
          value = value.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Specified = copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Specified = {
        copy(name = name.map(n => n.mapExpressions(fn)), value = fn(value))
      }

      /** @inheritdoc */
      override def toString: String =
        s"""
        |IR.CallArgument.Specified(
        |name = $name,
        |value = $value,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = name.toList :+ value

      /** @inheritdoc */
      override def showCode(indent: Int): String = {
        if (name.isDefined) {
          s"(${name.get.showCode(indent)} = ${value.showCode(indent)})"
        } else {
          s"${value.showCode(indent)}"
        }
      }
    }
  }

  // === Case Expression ======================================================

  /** The Enso case expression. */
  sealed trait Case extends Expression {

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Case

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Case

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Case
  }
  object Case {

    /** The main body of the Enso case expression.
      *
      * @param scrutinee the expression whose value is being matched on
      * @param branches the branches of the case expression
      * @param isNested if true, the flag indicates that the expr represents a desugared nested case
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Expr(
      scrutinee: Expression,
      branches: Seq[Branch],
      isNested: Boolean,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Case
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      def this(
        scrutinee: Expression,
        branches: Seq[Branch],
        location: Option[IdentifiedLocation],
        passData: MetadataStorage,
        diagnostics: DiagnosticStorage
      ) = {
        this(scrutinee, branches, false, location, passData, diagnostics)
      }

      /** Creates a copy of `this`.
        *
        * @param scrutinee the expression whose value is being matched on
        * @param branches the branches of the case expression
        * @param isNested if true, the flag indicates that the expr represents a desugared nested case
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        scrutinee: Expression                = scrutinee,
        branches: Seq[Branch]                = branches,
        isNested: Boolean                    = isNested,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Expr = {
        val res =
          Expr(scrutinee, branches, isNested, location, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Expr =
        copy(
          scrutinee = scrutinee.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          branches = branches.map(
            _.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            )
          ),
          isNested = isNested,
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Expr =
        copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Expr = {
        copy(
          scrutinee = fn(scrutinee),
          branches.map(_.mapExpressions(fn))
        )
      }

      /** @inheritdoc */
      override def toString: String =
        s"""
        |IR.Case.Expr(
        |scrutinee = $scrutinee,
        |branches = $branches,
        |isNested = $isNested,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = scrutinee :: branches.toList

      /** @inheritdoc */
      override def showCode(indent: Int): String = {
        val newIndent = indent + indentLevel
        val headerStr = s"case ${scrutinee.showCode(indent)} of"
        val branchesStr = branches
          .map(mkIndent(newIndent) + _.showCode(newIndent))
          .mkString("\n")

        s"$headerStr\n$branchesStr"
      }
    }

    object Expr {
      def apply(
        scrutinee: Expression,
        branches: Seq[Branch],
        location: Option[IdentifiedLocation]
      ): Expr =
        apply(
          scrutinee,
          branches,
          location,
          new MetadataStorage(),
          new DiagnosticStorage()
        )

      def apply(
        scrutinee: Expression,
        branches: Seq[Branch],
        location: Option[IdentifiedLocation],
        passData: MetadataStorage,
        diagnostics: DiagnosticStorage
      ): Expr = new Expr(scrutinee, branches, location, passData, diagnostics)
    }

    /** A branch in a case statement.
      *
      * @param pattern the pattern that attempts to match against the scrutinee
      * @param expression the expression that is executed if the pattern matches
      * @param terminalBranch the flag indicating whether the branch represents the final pattern to be checked
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Branch(
      pattern: Pattern,
      expression: Expression,
      terminalBranch: Boolean,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Case
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      def this(
        pattern: Pattern,
        expression: Expression,
        location: Option[IdentifiedLocation],
        passData: MetadataStorage,
        diagnostics: DiagnosticStorage
      ) = {
        this(pattern, expression, true, location, passData, diagnostics)
      }

      /** Creates a copy of `this`.
        *
        * @param pattern the pattern that attempts to match against the scrutinee
        * @param expression the expression that is executed if the pattern matches
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        pattern: Pattern                     = pattern,
        expression: Expression               = expression,
        terminalBranch: Boolean              = terminalBranch,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Branch = {
        val res = Branch(
          pattern,
          expression,
          terminalBranch,
          location,
          passData,
          diagnostics
        )
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Branch =
        copy(
          pattern = pattern.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          expression = expression.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          terminalBranch = terminalBranch,
          location       = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Branch =
        copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Branch = {
        copy(pattern = pattern.mapExpressions(fn), expression = fn(expression))
      }

      /** @inheritdoc */
      override def toString: String =
        s"""
        |IR.Case.Branch(
        |pattern = $pattern,
        |expression = $expression,
        |terminalBranch = $terminalBranch,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = List(pattern, expression)

      /** @inheritdoc */
      override def showCode(indent: Int): String = {
        val newIndent = indent + indentLevel
        val bodyStr = if (expression.isInstanceOf[ir.Expression.Block]) {
          s"\n${mkIndent(newIndent)}${expression.showCode(newIndent)}"
        } else {
          s"${expression.showCode(indent)}"
        }
        s"${pattern.showCode(indent)} -> $bodyStr"
      }
    }

    object Branch {
      def apply(
        pattern: Pattern,
        expression: Expression,
        location: Option[IdentifiedLocation]
      ): Branch =
        apply(
          pattern,
          expression,
          location,
          new MetadataStorage(),
          new DiagnosticStorage()
        )

      def apply(
        pattern: Pattern,
        expression: Expression,
        location: Option[IdentifiedLocation],
        passData: MetadataStorage,
        diagnostics: DiagnosticStorage
      ): Branch =
        new Branch(pattern, expression, location, passData, diagnostics)
    }
  }

  // === Comments =============================================================

  /** Enso comment entities. */
  sealed trait Comment extends Expression with ir.module.scope.Definition {

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Comment

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Comment

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Comment
  }
  object Comment {

    /** A documentation comment in the Enso source.
      *
      * @param doc the documentation entity
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Documentation(
      doc: String,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Comment
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param doc the documentation of `commented`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        doc: String                          = doc,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Documentation = {
        val res = Documentation(doc, location, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Documentation =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Documentation = copy(location = location)

      /** @inheritdoc */
      override def mapExpressions(
        fn: Expression => Expression
      ): Documentation = this

      /** @inheritdoc */
      override def toString: String =
        s"""
        |IR.Comment.Documentation(
        |doc = $doc,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = List()

      /** @inheritdoc */
      override def showCode(indent: Int): String =
        s"## $doc"
    }
  }

  /*
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
   */
  // ==========================================================================
  // === Extension Methods ====================================================
  // ==========================================================================

  /** This class adds an extension method to control how the pass data element
    * of the IR is printed.
    *
    * @param ir the IR to print the pass data for
    */
  implicit class ShowPassData(ir: IR) {

    /** Creates a string representation of the pass data for a given IR node.
      *
      * @return a string representation of the pass data for [[ir]]
      */
    def showPassData: String = {
      val metaString: Seq[String] =
        ir.passData.map((p, m) => (p, m.metadataName)).values.toSeq
      val alphabetical = metaString.sorted
      s"$alphabetical"
    }
  }

  /** Adds extension methods on strings to aid in writing custom to string
    * overrides.
    *
    * @param string the string to process
    */
  implicit class ToStringHelper(string: String) {

    /** Converts a multiline string to a single line
      *
      * @return [[string]], converted to a single line
      */
    def toSingleLine: String = {
      val lines = string.stripMargin.split("\n").toList.filterNot(_ == "")

      val body = lines.tail.dropRight(1).mkString(" ")

      s"${lines.head}$body${lines.last}"
    }
  }

  // ==========================================================================
  // === Useful Extension Methods =============================================
  // ==========================================================================

  /** Adds extension methods for working directly with the diagnostics on the
    * IR.
    *
    * @param ir the IR to add the methods to
    * @tparam T the concrete type of the IR
    */
  implicit class AsDiagnostics[T <: IR](ir: T) {

    /** Adds a new diagnostic entity to [[IR]].
      *
      * @param diagnostic the diagnostic to add
      * @return [[ir]] with added diagnostics
      */
    def addDiagnostic(diagnostic: Diagnostic): T = {
      ir.diagnostics.add(diagnostic)
      ir
    }
  }

  /** Adds extension methods for working directly with the metadata on the IR.
    *
    * @param ir the IR to add the methods to
    * @tparam T the concrete type of the IR
    */
  implicit class AsMetadata[T <: IR](ir: T) {

    /** Adds a metadata pair to the node metadata.
      *
      * This will overwrite any entry whose key matches [[MetadataPair#pass]].
      *
      * @param metadataPair the pair to add to the storage
      * @tparam K the concrete type of the pass
      */
    def updateMetadata[K <: ProcessingPass](
      metadataPair: MetadataPair[K]
    ): T = {
      ir.passData.update(metadataPair)
      ir
    }

    /** Gets the metadata for the specified pass.
      *
      * @param pass the pass to get the metadata for
      * @tparam K the concrete type of `pass`
      * @return the metadata for `pass`, if it exists
      */
    def getMetadata[K <: ProcessingPass](pass: K): Option[pass.Metadata] = {
      ir.passData.get(pass)
    }

    /** Unsafely gets the metadata for the specified pass, if it exists.
      *
      * @param pass the pass to get metadata for
      * @param msg the message to throw with if the unsafe get fails
      * @tparam K the concrete type of `pass`
      * @throws CompilerError if no metadata exists for `pass`
      * @return the metadata for `pass`, if it exists
      */
    @throws[CompilerError]
    def unsafeGetMetadata[K <: ProcessingPass](
      pass: ProcessingPass,
      msg: => String
    ): pass.Metadata = {
      ir.passData.getUnsafe(pass)(msg)
    }
  }

  /** Adds extension methods for working with lists of [[IR]].
    *
    * @param list the list
    * @tparam T the concrete IR type
    */
  implicit class ListAsIr[T <: IR](list: List[T]) {

    /** Calls [[IR.duplicate]] on the elements in [[list]].
      *
      * @param keepLocations whether or not locations should be kept in the
      *                      duplicated IR
      * @param keepMetadata whether or not the pass metadata should be kept in
      *                     the duplicated IR
      * @param keepDiagnostics whether or not the diagnostics should be kept in
      *                        the duplicated IR
      * @param keepIdentifiers whether or not the identifiers should be
      *                        regenerated in the duplicated IR
      * @return a duplicate of [[list]]
      */
    def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): List[T] = {
      list
        .map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        )
        .asInstanceOf[List[T]]
    }
  }
}
