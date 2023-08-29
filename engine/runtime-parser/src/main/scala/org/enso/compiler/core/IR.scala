package org.enso.compiler.core

import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.ir.{
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
import com.oracle.truffle.api.source.Source

import java.util.UUID
import scala.annotation.unused

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

  private def fileLocationFromSection(
    loc: IdentifiedLocation,
    source: Source
  ): String = {
    val section =
      source.createSection(loc.location.start, loc.location.length)
    val locStr =
      "" + section.getStartLine + ":" +
      section.getStartColumn + "-" +
      section.getEndLine + ":" +
      section.getEndColumn
    source.getName + "[" + locStr + "]"
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

  // === Diagnostics ==========================================================

  /** A representation of various kinds of diagnostic in the IR. */
  trait Diagnostic extends Serializable {

    /** @return a human-readable description of this error condition.
      */
    def message: String

    /** @return a human-readable description of this error condition, formatted for immediate reporting. */
    def formattedMessage: String = message

    /** The location at which the diagnostic occurs. */
    val location: Option[IdentifiedLocation]

    /** The important keys identifying identity of the diagnostic
      */
    def diagnosticKeys(): Array[Any]
  }
  object Diagnostic {

    /** Represents the various kinds of diagnostics in the IR. */
    sealed trait Kind
    object Kind {

      /** Diagnostics that should be reported during the static compilation
        * phase of execution.
        */
      sealed trait Static extends Kind

      /** Diagnostics that should remain at runtime for display during
        * interactive execution.
        */
      sealed trait Interactive extends Kind
    }
  }

  // === Warnings =============================================================

  /** A trait for all warnings in Enso's IR. */
  sealed trait Warning extends Diagnostic
  object Warning {

    /** Warnings about unused language entities. */
    sealed trait Unused extends Warning {
      val name: Name
    }
    object Unused {

      /** A warning about an unused function argument.
        *
        * @param name the name that is unused
        */
      sealed case class FunctionArgument(override val name: Name)
          extends Unused {
        override def message: String = s"Unused function argument ${name.name}."

        override def diagnosticKeys(): Array[Any] = Array(name.name)

        override def toString: String = s"Unused.FunctionArgument(${name.name})"

        override val location: Option[IdentifiedLocation] = name.location
      }

      sealed case class PatternBinding(override val name: Name) extends Unused {
        override def message: String = s"Unused pattern binding ${name.name}."

        override def diagnosticKeys(): Array[Any] = Array(name.name)

        override def toString: String = s"Unused.PatternBinding(${name.name})"

        override val location: Option[IdentifiedLocation] = name.location
      }

      /** A warning about an unused binding.
        *
        * @param name the name that is unused
        */
      sealed case class Binding(override val name: Name) extends Unused {
        override def message: String = s"Unused variable ${name.name}."

        override def diagnosticKeys(): Array[Any] = Array(name.name)

        override def toString: String = s"Unused.Binding(${name.name})"

        override val location: Option[IdentifiedLocation] = name.location
      }
    }

    /** Warnings for unreachable code. */
    sealed trait Unreachable extends Warning {
      val location: Option[IdentifiedLocation]
    }
    object Unreachable {

      /** A warning for unreachable branches in a case expression.
        *
        * @param location the location of the unreachable branches
        */
      sealed case class Branches(
        override val location: Option[IdentifiedLocation]
      ) extends Unreachable {
        val atLocation =
          if (location.isDefined) { s" at location ${location.get}" }
          else                    { ""                              }

        override def message: String = s"Unreachable case branches$atLocation."

        override def diagnosticKeys(): Array[Any] = Array(atLocation)
      }
    }

    case class DuplicatedImport(
      override val location: Option[IdentifiedLocation],
      originalImport: ir.module.scope.Import,
      symbolName: String,
      source: Source
    ) extends Warning {
      override def message: String = {
        val originalImportRepr =
          originalImport.location match {
            case Some(location) =>
              s"'${originalImport.showCode()}' in ${fileLocationFromSection(location, source)}"
            case None => originalImport.showCode()
          }
        s"Duplicated import of $symbolName. The original import is ${originalImportRepr}."
      }

      override def diagnosticKeys(): Array[Any] = Array()
    }

    /** A warning about a `@Tail_Call` annotation placed in a non-tail
      * position.
      * @param location the location of the annotated application
      */
    case class WrongTco(override val location: Option[IdentifiedLocation])
        extends Warning {
      override def message: String =
        "A @Tail_Call annotation was placed in a non-tail-call position."

      override def diagnosticKeys(): Array[Any] = Array()
    }

    /** A warning about a `@Builtin_Method` annotation placed in a method
      * with unexpected body.
      * @param location the location of the annotated application
      */
    case class WrongBuiltinMethod(
      override val location: Option[IdentifiedLocation]
    ) extends Warning {
      override def message: String =
        "A @Builtin_Method annotation allows only the name of the builtin node in the body."

      override def diagnosticKeys(): Array[Any] = Array()
    }

    /** A warning raised when a method is defined with a `self` parameter defined
      * not in the first position in the parameters' list.`
      *
      * @param ir the annotated application
      * @param paramPosition the reason why the annotation cannot be obeyed
      */
    case class WrongSelfParameterPos(
      funName: Name,
      ir: IR,
      paramPosition: Int
    ) extends Warning {
      override val location: Option[IdentifiedLocation] = ir.location
      override def message: String =
        s"${funName.name}: Self parameter should be declared as the first parameter. Instead its position is: ${paramPosition + 1}."

      override def diagnosticKeys(): Array[Any] =
        Array(ir.showCode(), paramPosition)
    }

    /** Warnings about shadowing names. */
    sealed trait Shadowed extends Warning {

      /** The [[IR]] shadowing the warned expression. */
      val shadower: IR
    }
    object Shadowed {

      /** A warning that a later-defined lambda parameter shadows an
        * earlier-defined lambda parameter.
        *
        * @param shadowedName the name being shadowed
        * @param shadower the expression shadowing `warnedExpr`
        * @param location the location at which the shadowing takes place
        */
      sealed case class FunctionParam(
        shadowedName: String,
        override val shadower: IR,
        override val location: Option[IdentifiedLocation]
      ) extends Shadowed {
        override def message: String =
          s"The argument $shadowedName is shadowed by $shadower"

        override def diagnosticKeys(): Array[Any] =
          Array(shadowedName, shadower)
      }

      /** A warning that a later-defined pattern variable shadows an
        * earlier-defined pattern variable.
        *
        * @param shadowedName the name being shadowed
        * @param shadower the expression shadowing `warnedExpr`
        * @param location the location at which the shadowing takes place
        */
      sealed case class PatternBinding(
        shadowedName: String,
        override val shadower: IR,
        override val location: Option[IdentifiedLocation]
      ) extends Shadowed {
        override def message: String =
          s"The pattern field $shadowedName is shadowed by $shadower."

        override def diagnosticKeys(): Array[Any] =
          Array(shadowedName, shadower)
      }

      /** A warning that a submodule is being shadowed by the type of the same name
        * therefore preventing the user from accessing the module via a qualified name.
        *
        * @param typename the type name shadowing the module
        * @param moduleName the module being shadowed
        * @param shadower the expression shadowing `moduleName`
        * @param location the location at which the shadowing takes place
        */
      sealed case class SyntheticModule(
        typeName: String,
        moduleName: Name.Qualified,
        override val shadower: IR,
        override val location: Option[IdentifiedLocation]
      ) extends Shadowed {
        override def message: String =
          s"""Declaration of type $typeName shadows module ${moduleName.name} making it inaccessible via a qualified name."""
        override def diagnosticKeys(): Array[Any] =
          Array(typeName, moduleName, shadower)

      }

      /** Used when the exported type of the module can name conflict with fully qualified names of submodules.
        *
        * @param name the module name
        * @param tpeName the name of the exported type leading to conflicts
        * @param firstConflict the name of the module that can be innaccessible because of the name conflict
        * @param shadower the export statement leading to a conflict
        * @param location the location of the export statement
        */
      sealed case class TypeInModuleNameConflicts(
        name: String,
        tpeName: String,
        firstConflict: String,
        override val shadower: IR,
        override val location: Option[IdentifiedLocation]
      ) extends Shadowed {
        override def message: String =
          s"The exported type `$tpeName` in `$name` module will cause name conflict " +
          s"when attempting to use a fully qualified name of the `$firstConflict` module."

        /** The important keys identifying identity of the diagnostic
          */
        override def diagnosticKeys(): Array[Any] =
          Array(name, tpeName, shadower)
      }
    }

    /** A warning raised when a call is annotated with `@Auto_Parallel`, but the
      * annotation cannot be obeyed.
      *
      * @param ir the annotated application
      * @param reason the reason why the annotation cannot be obeyed
      */
    case class FailedParallelism(
      ir: IR,
      reason: String
    ) extends Warning {
      override val location: Option[IdentifiedLocation] = ir.location
      override def message: String =
        s"The expression ${ir.showCode()} could not be parallelised: $reason."

      override def diagnosticKeys(): Array[Any] = Array(ir.showCode(), reason)
    }

    case class NonUnitTypeUsedOnValueLevel(ir: Name, context: String)
        extends Warning {

      /** @return a human-readable description of this error condition.
        */
      override def message: String =
        s"A non-unit type ${ir.name} is used on value level (in ${context})." +
        " This is probably an error."

      /** The location at which the diagnostic occurs. */
      override val location: Option[IdentifiedLocation] = ir.location

      /** The important keys identifying identity of the diagnostic
        */
      override def diagnosticKeys(): Array[Any] = Array(ir.name)
    }
  }

  // === Errors ===============================================================

  /** A trait for all errors in Enso's IR. */
  sealed trait Error
      extends Expression
      with ir.module.scope.Definition
      with Diagnostic {

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Error

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Error

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Error
  }
  object Error {

    /** An error resulting from processing conversion methods.
      *
      * @param storedIr the IR that contains the error
      * @param reason the explanation for the error
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler dianostics for this node
      */
    sealed case class Conversion(
      storedIr: IR,
      reason: Conversion.Reason,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Interactive
        with IRKind.Primitive
        with Name {
      override val name: String = "conversion_error"

      override def mapExpressions(fn: Expression => Expression): Conversion =
        this

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Conversion = {
        copy(storedIr = storedIr.setLocation(location))
      }

      /** Create a copy of `this`.
        *
        * @param storedIr the IR that contains the error
        * @param reason the explanation for the error
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler dianostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        storedIr: IR                   = storedIr,
        reason: Conversion.Reason      = reason,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): Conversion = {
        val res = Conversion(storedIr, reason, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean,
        keepMetadata: Boolean,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Conversion = {
        copy(
          storedIr = storedIr.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )
      }

      /** @inheritdoc */
      override def children: List[IR] = List(storedIr)

      /** @inheritdoc */
      override protected var id: Identifier = randomId

      /** @inheritdoc */
      override def showCode(indent: Int): String =
        s"(Error: ${storedIr.showCode(indent)})"

      /** @inheritdoc */
      override def message: String = reason.explain

      override def diagnosticKeys(): Array[Any] = Array(reason.explain)

      /** @inheritdoc */
      override val location: Option[IdentifiedLocation] = storedIr.location
    }
    object Conversion {

      /** The reason for the error. */
      sealed trait Reason {
        def explain: String
      }

      case object MissingArgs extends Reason {
        override def explain: String =
          "A conversion definition must have at least one argument."
      }

      case object UnsupportedSourceType extends Reason {
        override def explain: String =
          "Arbitrary expressions are not yet supported as source types."
      }

      case class MissingSourceType(argName: String) extends Reason {
        override def explain: String =
          s"The argument `$argName` does not define a source type."
      }

      case class MissingSelfParam(argName: String) extends Reason {
        override def explain: String =
          s"""|Conversion definition must have an explicit `self` parameter in the first position.
              |Got `$argName` instead.""".stripMargin
      }

      case class NonDefaultedArgument(argName: String) extends Reason {
        override def explain: String =
          s"Additional arguments in a conversion must have a default, but " +
          s"`$argName` does not."
      }

      case class SuspendedSourceArgument(argName: String) extends Reason {
        override def explain: String =
          s"The `that` type argument in a conversion (here $argName) cannot " +
          s"be suspended."
      }

      case class InvalidSourceArgumentName(argName: String) extends Reason {
        override def explain: String =
          s"The source type argument must be ignored or named `that`, but" +
          s" ${argName} was found."
      }
    }

    /** A representation of an error resulting from name resolution.
      *
      * @param originalName the original name that could not be resolved
      * @param reason the cause of this error
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Resolution(
      originalName: Name,
      reason: Resolution.Reason,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Interactive
        with IRKind.Primitive
        with Name {
      override val name: String = originalName.name

      override def mapExpressions(fn: Expression => Expression): Resolution =
        this

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Resolution =
        copy(originalName = originalName.setLocation(location))

      /** Creates a copy of `this`.
        *
        * @param originalName the original name that could not be resolved
        * @param reason the cause of this error
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        originalName: Name             = originalName,
        reason: Resolution.Reason      = reason,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): Resolution = {
        val res = Resolution(originalName, reason, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Resolution =
        copy(
          originalName = originalName
            .duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      /** @inheritdoc */
      override def children: List[IR] = List(originalName)

      /** @inheritdoc */
      override protected var id: Identifier = randomId

      /** @inheritdoc */
      override def showCode(indent: Int): String = originalName.showCode(indent)

      /** @inheritdoc */
      override def message: String = reason.explain(originalName)

      /** @inheritdoc */
      override def formattedMessage: String = s"${message}."

      override def diagnosticKeys(): Array[Any] = Array(reason)

      /** @inheritdoc */
      override val location: Option[IdentifiedLocation] = originalName.location
    }

    object Resolution {

      /** A representation of a symbol resolution error.
        */
      sealed trait Reason {
        def explain(originalName: Name): String
      }

      case object UnresolvedSequenceMacro extends Reason {
        override def explain(originalName: Name): String =
          "No definition for the sequence macro could be found. Try" +
          " importing the default definition from the Standard.Base module"
      }

      /** An error coming from an unknown annotation name.
        */
      case object UnknownAnnotation extends Reason {
        override def explain(originalName: Name): String =
          s"The annotation ${originalName.name} is not defined"
      }

      /** An error coming from a tail call annotation placed in a syntactically
        * incorrect position.
        */
      case object UnexpectedAnnotation extends Reason {
        override def explain(originalName: Name): String =
          s"Unexpected ${originalName.name} annotation. This annotation can " +
          s"only be used with function applications"
      }

      /** An error coming from an unexpected occurence of a polyglot symbol.
        *
        * @param context the description of a context in which the error
        *                happened.
        */
      case class UnexpectedPolyglot(context: String) extends Reason {
        override def explain(originalName: Name): String =
          s"The name ${originalName.name} resolved to a polyglot symbol, " +
          s"but polyglot symbols are not allowed in $context"
      }

      /** An error coming from an unexpected occurence of a constructor.
        *
        * @param context the description of a context in which the error
        *                happened.
        */
      case class UnexpectedConstructor(context: String) extends Reason {
        override def explain(originalName: Name): String =
          s"The name ${originalName.name} resolved to a constructor, " +
          s"but constructors are not allowed in $context"
      }

      /** An error coming from an unexpected occurence of a static method.
        *
        * @param context the description of a context in which the error
        *                happened.
        */
      case class UnexpectedMethod(context: String) extends Reason {
        override def explain(originalName: Name): String =
          s"The name ${originalName.name} resolved to a method, " +
          s"but methods are not allowed in $context"
      }

      /** An error coming from an unexpected occurence of a module.
        *
        * @param context the description of a context in which the error
        *                happened.
        */
      case class UnexpectedModule(context: String) extends Reason {
        override def explain(originalName: Name): String =
          s"The name ${originalName.name} resolved to a module, " +
          s"but modules are not allowed in $context"
      }

      /** An error coming from an unexpected occurence of a type.
        *
        * @param context the description of a context in which the error
        *                happened.
        */
      case class UnexpectedType(context: String) extends Reason {
        override def explain(originalName: Name): String =
          s"The name ${originalName.name} resolved to a type, " +
          s"but types are not allowed in $context"
      }

      /** An error coming from usage of an undefined variable name.
        */
      case object VariableNotInScope extends Reason {
        override def explain(originalName: Name): String =
          s"Variable `${originalName.name}` is not defined"
      }

      /** An error coming from name resolver.
        *
        * @param err the original error.
        */
      case class ResolverError(private val explain: ExplainResolution)
          extends Reason {

        /** Provides a human-readable explanation of the error.
          * @param originalName the original unresolved name.
          * @return a human-readable message.
          */
        override def explain(originalName: Name): String =
          this.explain.explain(originalName)
      }

      trait ExplainResolution {
        def explain(originalName: Name): String
      }

      case class MissingLibraryImportInFQNError(namespace: String)
          extends Reason {
        override def explain(originalName: Name): String =
          s"Fully qualified name references a library $namespace.${originalName.name} but an import statement for it is missing"
      }

    }

    /** A representation of an error resulting from wrong pattern matches.
      *
      * @param originalPattern pattern that resulted in the error
      * @param reason the cause of this error
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @return a copy of `this`, updated with the specified values
      */
    sealed case class Pattern(
      originalPattern: ir.Pattern,
      reason: Pattern.Reason,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Interactive
        with ir.Pattern {
      override def mapExpressions(fn: Expression => Expression): Pattern =
        copy(originalPattern = originalPattern.mapExpressions(fn))

      override def setLocation(location: Option[IdentifiedLocation]): Pattern =
        copy(originalPattern = originalPattern.setLocation(location))

      /** Creates a copy of `this`.
        *
        * @param originalPattern the pattern that resulted in the error
        * @param reason the cause of this error
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        originalPattern: ir.Pattern    = originalPattern,
        reason: Pattern.Reason         = reason,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): Pattern = {
        val res = Pattern(originalPattern, reason, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Pattern =
        copy(
          originalPattern = originalPattern
            .duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      override def message: String = reason.explain

      override def diagnosticKeys(): Array[Any] = Array(reason)

      override val location: Option[IdentifiedLocation] =
        originalPattern.location

      override def children: List[IR] = List(originalPattern)

      override protected var id: Identifier = randomId

      override def showCode(indent: Int): String =
        originalPattern.showCode(indent)
    }

    object Pattern {

      /** A representation of the reason the pattern is erroneous.
        */
      sealed trait Reason {

        /** Provides a human-readable explanation of the error.
          * @return
          */
        def explain: String
      }

      /** A reason for pattern failing due to wrong arity.
        *
        * @param consName the constructor name.
        * @param expected expected field count.
        * @param actual actual field count.
        */
      case class WrongArity(consName: String, expected: Int, actual: Int)
          extends Reason {
        override def explain: String =
          s"Wrong number of fields when matching on $consName." +
          s" Expected $expected fields, but provided $actual"
      }
    }

    /** A representation of an Enso syntax error.
      *
      * @param at the error location
      * @param reason the cause of this error
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Syntax(
      at: IdentifiedLocation,
      reason: Syntax.Reason,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Interactive
        with ir.module.scope.Definition
        with ir.module.scope.Export
        with ir.module.scope.Import
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param ast the error location
        * @param reason the cause of this error
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        at: IdentifiedLocation         = at,
        reason: Syntax.Reason          = reason,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): Syntax = {
        val res = Syntax(at, reason, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        @unused keepLocations: Boolean = true,
        keepMetadata: Boolean          = true,
        keepDiagnostics: Boolean       = true,
        keepIdentifiers: Boolean       = false
      ): Syntax =
        copy(
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Syntax =
        this

      /** @inheritdoc */
      override val location: Option[IdentifiedLocation] = Option(at)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Syntax = this

      /** @inheritdoc */
      override def toString: String =
        s"""
        |IR.Error.Syntax(
        |at = $at,
        |reason = $reason,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = List()

      /** @inheritdoc */
      override def message: String = reason.explanation

      /** @inheritdoc */
      override def formattedMessage: String = s"${message}."

      override def diagnosticKeys(): Array[Any] = Array(reason)

      /** @inheritdoc */
      override def showCode(indent: Int): String = "Syntax_Error"
    }
    object Syntax {

      /** A common type for all syntax errors expected by the language.
        */
      sealed trait Reason {

        /** @return a human-readable description of the error.
          */
        def explanation: String
      }

      case object SuspendedArgInAtom extends Reason {
        override def explanation: String =
          "Atoms may not have suspended arguments"
      }

      case class InvalidEscapeSequence(lit: String) extends Reason {
        override def explanation: String = s"Invalid escape sequence $lit"
      }

      case object InvalidBaseInDecimalLiteral extends Reason {
        override def explanation: String =
          "Cannot change base of the fractional part of a number literal"
      }

      case class InvalidBase(base: String) extends Reason {
        override def explanation: String =
          s"$base is not a valid numeric base"
      }

      case class InvalidNumberForBase(base: String, number: String)
          extends Reason {
        override def explanation: String =
          s"$number is not valid in $base"
      }

      case class UnsupportedSyntax(syntaxName: String) extends Reason {
        override def explanation: String =
          s"Syntax is not supported yet: $syntaxName"
      }

      case object InvalidUnderscore extends Reason {
        override def explanation: String =
          s"Invalid use of _"
      }

      case object InvalidPattern extends Reason {
        override def explanation: String =
          s"Cannot define a pattern outside a pattern context"
      }

      case class InvalidImport(
        message: String = "Imports must have a valid module path"
      ) extends Reason {
        override def explanation: String =
          s"Invalid Import: $message"
      }

      case class InvalidExport(
        message: String = "Exports must have a valid module path"
      ) extends Reason {
        override def explanation: String =
          s"Invalid Export: $message"
      }

      case object InvalidStandaloneSignature extends Reason {
        override def explanation: String =
          s"Invalid stand-alone signature expression"
      }

      case class MethodDefinedInline(methodName: String) extends Reason {
        override def explanation: String =
          s"Cannot define $methodName, methods are not supported in the " +
          s"inline flow"
      }

      case object UnexpectedDeclarationInType extends Reason {
        override def explanation: String =
          "Unexpected declaration in the body of a type"
      }

      case object InvalidTypeDefinition extends Reason {
        override def explanation: String =
          "Invalid definition of a type"
      }

      case class TypeDefinedInline(typeName: String) extends Reason {
        override def explanation: String =
          s"Cannot define $typeName, type definitions are not supported " +
          s"in the inline flow"
      }

      case object EmptyParentheses extends Reason {
        override def explanation: String =
          "Parentheses can't be empty"
      }

      case object UnexpectedExpression extends Reason {
        override def explanation: String = "Unexpected expression"
      }

      case object AmbiguousExpression extends Reason {
        override def explanation: String = "Ambiguous expression"
      }

      case object InvalidSelfArgUsage extends Reason {
        override def explanation: String =
          "Self argument cannot be used in static methods"
      }

      case object UnrecognizedToken extends Reason {
        override def explanation: String = "Unrecognized token"
      }

      case object InvalidSuffix extends Reason {
        override def explanation: String = "Invalid suffix"
      }

      case object UnclosedTextLiteral extends Reason {
        override def explanation: String = "Unclosed text literal"
      }

      case object NamedArgInSection extends Reason {
        override def explanation: String = "Named argument in operator section"
      }

      case object NamedArgInOperator extends Reason {
        override def explanation: String = "Named argument in operator section"
      }

      case object InvalidOperatorName extends Reason {
        override def explanation: String = "Invalid operator name"
      }

      case class InvalidForeignDefinition(details: String) extends Reason {
        override def explanation: String =
          s"Invalid foreign definition. $details"
      }
    }

    /** A representation of an invalid piece of IR.
      *
      * @param ir the IR that is invalid
      * @param passData any annotations from compiler passes
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class InvalidIR(
      ir: IR,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Static
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param ir the IR that is invalid
        * @param passData any annotations from compiler passes
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        ir: IR                         = ir,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): InvalidIR = {
        val res = InvalidIR(ir, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): InvalidIR =
        copy(
          ir = ir.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(
        location: Option[IdentifiedLocation]
      ): InvalidIR = this

      /** @inheritdoc */
      override val location: Option[IdentifiedLocation] = ir.location

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): InvalidIR =
        this

      /** @inheritdoc */
      override def toString: String =
        s"""
        |IR.Error.InvalidIR(
        |ir = $ir,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = List(ir)

      /** @inheritdoc */
      override def message: String =
        "InvalidIR: Please report this as a compiler bug."

      override def diagnosticKeys(): Array[Any] = Array()

      /** @inheritdoc */
      override def showCode(indent: Int): String = "Invalid_Ir"
    }

    /** Errors pertaining to the redefinition of language constructs that are
      * not allowed to be.
      */
    sealed trait Redefined extends Error {

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Redefined

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Redefined

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Redefined
    }
    object Redefined {

      /** An error representing the redefinition or incorrect positioning of
        * the `self` argument to methods.
        *
        * @param location the source location of the error
        * @param passData the pass metadata for this node
        * @param diagnostics compiler diagnostics associated with the node
        */
      sealed case class SelfArg(
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `self`.
          *
          * @param location the source location of the error
          * @param passData the pass metadata for this node
          * @param diagnostics compiler diagnostics associated with the node
          * @param id the node's identifier
          * @return a copy of `this`, with the specified values updated
          */
        def copy(
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): SelfArg = {
          val res = SelfArg(location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): SelfArg =
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
        ): SelfArg = copy(location = location)

        /** @inheritdoc */
        override def mapExpressions(fn: Expression => Expression): SelfArg =
          this

        /** @inheritdoc */
        override def message: String =
          "Methods must have only one definition of the `this` argument, and " +
          "it must be the first."

        override def diagnosticKeys(): Array[Any] = Array()

        /** @inheritdoc */
        override def children: List[IR] = List()

        /** @inheritdoc */
        override def showCode(indent: Int): String = "(Redefined This_Arg)"
      }

      /** An error representing the redefinition of a conversion in a given
        * module. This is also known as a method overload.
        *
        * @param targetType the name of the atom the conversion was being
        *                 redefined on
        * @param sourceType the source type for the conversion
        * @param location the location in the source to which this error
        *                 corresponds
        * @param passData the pass metadata for the error
        * @param diagnostics any diagnostics associated with this error.
        */
      sealed case class Conversion(
        targetType: Option[Name],
        sourceType: Name,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with ir.module.scope.Definition
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param targetType the name of the atom the conversion was being
          *                 redefined on
          * @param sourceType the source type for the conversion
          * @param location the location in the source to which this error
          *                 corresponds
          * @param passData the pass metadata for the error
          * @param diagnostics any diagnostics associated with this error.
          * @param id the identifier for the node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          targetType: Option[Name]             = targetType,
          sourceType: Name                     = sourceType,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Conversion = {
          val res =
            Conversion(targetType, sourceType, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Conversion =
          copy(
            targetType = targetType.map(
              _.duplicate(
                keepLocations,
                keepMetadata,
                keepDiagnostics,
                keepIdentifiers
              )
            ),
            sourceType = sourceType
              .duplicate(
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
        ): Conversion =
          copy(location = location)

        /** @inheritdoc */
        override def message: String =
          s"Method overloads are not supported: ${targetType.map(_.name + ".").getOrElse("")}from " +
          s"${sourceType.showCode()} is defined multiple times in this module."

        override def diagnosticKeys(): Array[Any] = targetType
          .map(_.name :: sourceType.showCode() :: Nil)
          .getOrElse(sourceType.showCode() :: Nil)
          .toArray

        /** @inheritdoc */
        override def mapExpressions(fn: Expression => Expression): Conversion =
          this

        /** @inheritdoc */
        override def toString: String =
          s"""
             |IR.Error.Redefined.Method(
             |targetType = $targetType,
             |sourceType = $sourceType,
             |location = $location,
             |passData = ${this.showPassData},
             |diagnostics = $diagnostics,
             |id = $id
             |)
             |""".stripMargin

        /** @inheritdoc */
        override def children: List[IR] =
          targetType
            .map(_ :: sourceType :: Nil)
            .getOrElse(sourceType :: Nil)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(Redefined (Conversion ${targetType.map(_.showCode() + ".").getOrElse("")}from $sourceType))"
      }

      /** An error representing the redefinition of a method in a given module.
        * This is also known as a method overload.
        *
        * @param atomName the name of the atom the method was being redefined on
        * @param methodName the method name being redefined on `atomName`
        * @param location the location in the source to which this error
        *                 corresponds
        * @param passData the pass metadata for the error
        * @param diagnostics any diagnostics associated with this error.
        */
      sealed case class Method(
        atomName: Option[Name],
        methodName: Name,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with ir.module.scope.Definition
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param atomName the name of the atom the method was being redefined on
          * @param methodName the method name being redefined on `atomName`
          * @param location the location in the source to which this error
          *                 corresponds
          * @param passData the pass metadata for the error
          * @param diagnostics any diagnostics associated with this error.
          * @param id the identifier for the node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          atomName: Option[Name]               = atomName,
          methodName: Name                     = methodName,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Method = {
          val res =
            Method(atomName, methodName, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Method =
          copy(
            atomName = atomName.map(
              _.duplicate(
                keepLocations,
                keepMetadata,
                keepDiagnostics,
                keepIdentifiers
              )
            ),
            methodName = methodName
              .duplicate(
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
        override def setLocation(location: Option[IdentifiedLocation]): Method =
          copy(location = location)

        /** @inheritdoc */
        override def message: String =
          s"Method overloads are not supported: ${atomName.map(_.name + ".").getOrElse("")}" +
          s"${methodName.name} is defined multiple times in this module."

        override def diagnosticKeys(): Array[Any] = {
          atomName
            .map(_.name :: methodName.name :: Nil)
            .getOrElse(methodName.name :: Nil)
            .toArray
        }

        /** @inheritdoc */
        override def mapExpressions(fn: Expression => Expression): Method = this

        /** @inheritdoc */
        override def toString: String =
          s"""
             |IR.Error.Redefined.Method(
             |atomName = $atomName,
             |methodName = $methodName,
             |location = $location,
             |passData = ${this.showPassData},
             |diagnostics = $diagnostics,
             |id = $id
             |)
             |""".stripMargin

        /** @inheritdoc */
        override def children: List[IR] =
          atomName
            .map(_ :: methodName :: Nil)
            .getOrElse(methodName :: Nil)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(Redefined (Method ${atomName.map(_.showCode() + ".").getOrElse("")}$methodName))"
      }

      /** An error representing the redefinition of a method in a given module,
        * when the module defines a method with the same name as an atom.
        * This is also known as a name clash.
        *
        * @param atomName the name of the atom that clashes with the method
        * @param methodName the method name being redefined in the module
        * @param location the location in the source to which this error
        *                 corresponds
        * @param passData the pass metadata for the error
        * @param diagnostics any diagnostics associated with this error.
        */
      sealed case class MethodClashWithAtom(
        atomName: Name,
        methodName: Name,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with ir.module.scope.Definition
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param atomName the name of the atom that clashes with the method
          * @param methodName the method name being redefined in the module
          * @param location the location in the source to which this error
          *                 corresponds
          * @param passData the pass metadata for the error
          * @param diagnostics any diagnostics associated with this error.
          * @param id the identifier for the node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          atomName: Name                       = atomName,
          methodName: Name                     = methodName,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): MethodClashWithAtom = {
          val res = MethodClashWithAtom(
            atomName,
            methodName,
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
        ): MethodClashWithAtom =
          copy(
            atomName = atomName.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            methodName = methodName
              .duplicate(
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
        ): MethodClashWithAtom =
          copy(location = location)

        /** @inheritdoc */
        override def message: String =
          s"Method definitions with the same name as atoms are not supported. " +
          s"Method ${methodName.name} clashes with the atom ${atomName.name} in this module."

        override def diagnosticKeys(): Array[Any] =
          Array(methodName.name, atomName.name)

        /** @inheritdoc */
        override def mapExpressions(
          fn: Expression => Expression
        ): MethodClashWithAtom =
          this

        /** @inheritdoc */
        override def toString: String =
          s"""
             |IR.Error.Redefined.MethodClashWithAtom(
             |atomName = $atomName,
             |methodName = $methodName,
             |location = $location,
             |passData = ${this.showPassData},
             |diagnostics = $diagnostics,
             |id = $id
             |)
             |""".stripMargin

        /** @inheritdoc */
        override def children: List[IR] = List(atomName, methodName)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(Redefined (MethodClash $atomName $methodName))"
      }

      /** An error representing the redefinition of an atom in a given module.
        *
        * @param typeName the name of the atom being redefined
        * @param location the location in the source to which this error
        *                 corresponds
        * @param passData the pass metadata for the error
        * @param diagnostics any diagnostics associated with this error.
        */
      sealed case class Type(
        typeName: Name,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with ir.module.scope.Definition
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param atomName the name of the atom the method was being redefined
          *                 on
          * @param location the location in the source to which this error
          *                 corresponds
          * @param passData the pass metadata for the error
          * @param diagnostics any diagnostics associated with this error.
          * @param id the identifier for the node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          atomName: Name                       = typeName,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Type = {
          val res =
            Type(atomName, location, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): Type =
          copy(
            atomName = typeName.duplicate(
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
        override def setLocation(location: Option[IdentifiedLocation]): Type =
          copy(location = location)

        /** @inheritdoc */
        override def message: String =
          s"Redefining atoms is not supported: ${typeName.name} is " +
          s"defined multiple times in this module."

        override def diagnosticKeys(): Array[Any] = Array(typeName.name)

        /** @inheritdoc */
        override def mapExpressions(fn: Expression => Expression): Type = this

        /** @inheritdoc */
        override def toString: String =
          s"""
             |IR.Error.Redefined.Atom(
             |atomName = $typeName,
             |location = $location,
             |passData = ${this.showPassData},
             |diagnostics = $diagnostics,
             |id = $id
             |)
             |""".stripMargin

        /** @inheritdoc */
        override def children: List[IR] = List(typeName)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(Redefined (Atom $typeName))"
      }

      /** An error representing the redefinition of a binding in a given scope.
        *
        * While bindings in child scopes are allowed to _shadow_ bindings in
        * parent scopes, a binding cannot be redefined within a given scope.
        *
        * @param invalidBinding the invalid binding
        * @param passData the pass metadata for the error
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Binding(
        invalidBinding: ir.Expression.Binding,
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param invalidBinding the invalid binding
          * @param passData the pass metadata for the error
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          invalidBinding: ir.Expression.Binding = invalidBinding,
          passData: MetadataStorage             = passData,
          diagnostics: DiagnosticStorage        = diagnostics,
          id: Identifier                        = id
        ): Binding = {
          val res = Binding(invalidBinding, passData, diagnostics)
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
            invalidBinding = invalidBinding
              .duplicate(
                keepLocations,
                keepMetadata,
                keepDiagnostics,
                keepIdentifiers
              ),
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = if (keepIdentifiers) id else randomId
          )

        /** @inheritdoc */
        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Binding = this

        /** @inheritdoc */
        override val location: Option[IdentifiedLocation] =
          invalidBinding.location

        /** @inheritdoc */
        override def mapExpressions(fn: Expression => Expression): Binding =
          this

        /** @inheritdoc */
        override def toString: String =
          s"""
             |IR.Error.Redefined.Binding(
             |invalidBinding = $invalidBinding,
             |location = $location,
             |passData = ${this.showPassData},
             |diagnostics = $diagnostics,
             |id = $id
             |)
             |""".stripMargin

        /** @inheritdoc */
        override def children: List[IR] = List(invalidBinding)

        /** @inheritdoc */
        override def message: String =
          s"Variable ${invalidBinding.name.name} is being redefined."

        override def diagnosticKeys(): Array[Any] = Array(
          invalidBinding.name.name
        )

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(Redefined (Binding $invalidBinding))"
      }
    }

    /** A trait for errors about unexpected language constructs. */
    sealed trait Unexpected extends Error {

      /** The unexpected construct. */
      val ir: IR

      /** The name of the unexpected entity. */
      val entity: String

      override val location: Option[IdentifiedLocation] = ir.location

      /** @inheritdoc */
      override def message: String = s"Unexpected $entity."

      /** @inheritdoc */
      override def diagnosticKeys(): Array[Any] = Array(entity)

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): Unexpected

      /** @inheritdoc */
      override def setLocation(location: Option[IdentifiedLocation]): Unexpected

      /** @inheritdoc */
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true,
        keepIdentifiers: Boolean = false
      ): Unexpected
    }
    object Unexpected {

      /** An error representing a type signature not associated with a
        * binding of some kind.
        *
        * @param ir the erroneous signature
        * @param passData any pass metadata associated with this node
        * @param diagnostics any compiler diagnostics for this node
        */
      sealed case class TypeSignature(
        override val ir: IR,
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Unexpected
          with IRKind.Primitive
          with org.enso.compiler.core.ir.module.scope.Definition {
        override val entity: String = "type signature"

        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param ir the erroneous signature
          * @param passData any pass metadata associated with this node
          * @param diagnostics any compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          ir: IR                         = ir,
          passData: MetadataStorage      = passData,
          diagnostics: DiagnosticStorage = diagnostics,
          id: Identifier                 = id
        ): TypeSignature = {
          val res = TypeSignature(ir, passData, diagnostics)
          res.id = id
          res
        }

        /** @inheritdoc */
        override def mapExpressions(
          fn: Expression => Expression
        ): TypeSignature = this

        /** @inheritdoc */
        override def setLocation(
          location: Option[IdentifiedLocation]
        ): TypeSignature = this

        /** @inheritdoc */
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true,
          keepIdentifiers: Boolean = false
        ): TypeSignature =
          copy(
            ir = ir.duplicate(
              keepLocations,
              keepMetadata,
              keepDiagnostics,
              keepIdentifiers
            ),
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = if (keepIdentifiers) id else randomId
          )

        /** @inheritdoc */
        override def children: List[IR] = List(ir)

        /** @inheritdoc */
        override def showCode(indent: Int): String =
          s"(Unexpected.TypeSignature ${ir.showCode(indent)})"
      }
    }

    object ImportExport {

      /** A reason for a statement being erroneous.
        */
      sealed trait Reason {

        /** @return  A human-readable description of the error.
          */
        def message: String
      }

      /** Used when the `project` keyword is used in an impossible position.
        * @param statementType the type of statement being affected, see the
        *                      implementation for its grammatical use.
        */
      case class ProjectKeywordUsedButNotInProject(statementType: String)
          extends Reason {
        override def message: String =
          s"The `project` keyword was used in an $statementType statement," +
          " but the module does not belong to a project."
      }

      /** Used when an import statement triggers loading of a package that could
        * not be loaded.
        *
        * @param name the module name.
        */
      case class PackageCouldNotBeLoaded(name: String, reason: String)
          extends Reason {
        override def message: String = s"Package containing the module $name" +
          s" could not be loaded: $reason"
      }

      /** Used when an import statement refers to a module that does not exist.
        * @param name the module name.
        */
      case class ModuleDoesNotExist(name: String) extends Reason {
        override def message: String = s"The module $name does not exist."
      }

      case class TypeDoesNotExist(
        typeName: String,
        moduleName: String
      ) extends Reason {
        override def message: String =
          s"The type $typeName does not exist in module $moduleName"
      }

      case class SymbolDoesNotExist(
        symbolName: String,
        moduleName: String
      ) extends Reason {
        override def message: String =
          s"The symbol $symbolName (module or type) does not exist in module $moduleName."
      }

      case class NoSuchConstructor(
        typeName: String,
        constructorName: String
      ) extends Reason {
        override def message: String =
          s"No such constructor ${constructorName} in type $typeName"
      }

      /** Represents an ambiguous import resolution error, where the same symbol is imported more than once refereing
        * to different objects. The objects are represented by their physical path in the project.
        * @param originalImport the original import statement.
        * @param originalSymbolPath the original symbol path.
        * @param symbolName the symbol name that is ambiguous.
        * @param symbolPath the symbol path that is different than [[originalSymbolPath]].
        * @param source Location of the original import.
        */
      case class AmbiguousImport(
        originalImport: ir.module.scope.Import,
        originalSymbolPath: String,
        symbolName: String,
        symbolPath: String,
        source: Source
      ) extends Reason {
        override def message: String = {
          val originalImportRepr =
            originalImport.location match {
              case Some(location) =>
                fileLocationFromSection(location, source)
              case None => originalImport.showCode()
            }
          s"Symbol '$symbolName' resolved ambiguously to '$symbolPath' in the import Statement. " +
          s"The symbol was first resolved to '$originalSymbolPath' in the import statement '$originalImportRepr'."
        }

      }
    }

    /** An erroneous import or export statement.
      *
      * @param ir the original statement
      * @param reason the reason it's erroneous
      * @param passData the pass data
      * @param diagnostics the attached diagnostics
      */
    sealed case class ImportExport(
      ir: IR,
      reason: ImportExport.Reason,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Interactive
        with org.enso.compiler.core.ir.module.scope.Import
        with org.enso.compiler.core.ir.module.scope.Export
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param ir the original IR
        * @param reason the cause of this error
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        ir: IR                         = ir,
        reason: ImportExport.Reason    = reason,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): ImportExport = {
        val res = ImportExport(ir, reason, passData, diagnostics)
        res.id = id
        res
      }

      /** @inheritdoc */
      override def duplicate(
        @unused keepLocations: Boolean = true,
        keepMetadata: Boolean          = true,
        keepDiagnostics: Boolean       = true,
        keepIdentifiers: Boolean       = false
      ): ImportExport =
        copy(
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = if (keepIdentifiers) id else randomId
        )

      /** @inheritdoc */
      override def setLocation(
        location: Option[IdentifiedLocation]
      ): ImportExport =
        this

      /** @inheritdoc */
      override val location: Option[IdentifiedLocation] = ir.location

      /** @inheritdoc */
      override def mapExpressions(fn: Expression => Expression): ImportExport =
        this

      /** @inheritdoc */
      override def toString: String =
        s"""
           |IR.Error.ImportExport(
           |ir = $ir,
           |reason = $reason,
           |location = $location,
           |passData = ${this.showPassData},
           |diagnostics = $diagnostics,
           |id = $id
           |)
           |""".toSingleLine

      /** @inheritdoc */
      override def children: List[IR] = List(ir)

      /** @inheritdoc */
      override def message: String = reason.message

      override def diagnosticKeys(): Array[Any] = Array(reason)

      /** @inheritdoc */
      override def showCode(indent: Int): String = "Import_Export_Error"
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
    def addDiagnostic(diagnostic: IR.Diagnostic): T = {
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
