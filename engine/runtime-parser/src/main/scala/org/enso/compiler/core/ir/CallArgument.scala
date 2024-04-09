package org.enso.compiler.core.ir

import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}

import java.util.UUID

/** Call-site arguments in Enso. */
sealed trait CallArgument extends IR {

  /** The name of the argument, if present. */
  val name: Option[Name]

  /** The expression of the argument, if present. */
  val value: Expression

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): CallArgument

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
    location: Option[IdentifiedLocation],
    passData: MetadataStorage      = new MetadataStorage(),
    diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends CallArgument
      with IRKind.Primitive
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param name              the name of the argument being called, if present
      * @param value             the expression being passed as the argument's value
      * @param location          the source location that the node corresponds to
      * @param shouldBeSuspended whether or not the argument should be passed
      *                          suspended
      * @param passData          the pass metadata associated with this node
      * @param diagnostics       compiler diagnostics for this node
      * @param id                the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: Option[Name]                   = name,
      value: Expression                    = value,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
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
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Specified = copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Specified = {
      copy(name = name.map(n => n.mapExpressions(fn)), value = fn(value))
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |CallArgument.Specified(
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
