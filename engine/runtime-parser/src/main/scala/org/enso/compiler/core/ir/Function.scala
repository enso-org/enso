package org.enso.compiler.core.ir

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}
import org.enso.persist.Persistance

import java.util.UUID

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

  /** Whether the method is project-private.
    */
  val isPrivate: Boolean
}

object Function {

  /** The primitive function type in Enso: `->`.
    *
    * It should be noted that while the _surface_ language does not support
    * multi-argument lambdas, our internal representation does so to allow for
    * better optimisation.
    *
    * @param arguments the arguments to the lambda
    * @param bodyReference the body of the lambda, stored as a reference to ensure laziness of storage
    * @param identifiedLocation the source location that the node corresponds to
    * @param canBeTCO whether or not the function can be tail-call optimised
    * @param passData the pass metadata associated with this node
    */
  sealed case class Lambda(
    override val arguments: List[DefinitionArgument],
    bodyReference: Persistance.Reference[Expression],
    override val identifiedLocation: IdentifiedLocation,
    override val canBeTCO: Boolean,
    override val passData: MetadataStorage
  ) extends Function
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    def this(
      arguments: List[DefinitionArgument],
      body: Expression,
      identifiedLocation: IdentifiedLocation,
      canBeTCO: Boolean         = true,
      passData: MetadataStorage = new MetadataStorage()
    ) = {
      this(
        arguments,
        Persistance.Reference.of(body, true),
        identifiedLocation,
        canBeTCO,
        passData
      )
    }

    def this(
      ir: expression.Case.Expr,
      arguments: List[DefinitionArgument],
      body: Expression,
      identifiedLocation: IdentifiedLocation
    ) = {
      this(
        arguments,
        Persistance.Reference.of(body, true),
        identifiedLocation,
        true,
        ir.passData.duplicate()
      )
      diagnostics = ir.diagnostics
    }

    override lazy val body: Expression = bodyReference.get(classOf[Expression])

    override val isPrivate: Boolean = false

    /** Creates a copy of `this`.
      *
      * @param arguments   the arguments to the lambda
      * @param body        the body of the lambda
      * @param location    the source location that the node corresponds to
      * @param canBeTCO    whether or not the function can be tail-call optimised
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      arguments: List[DefinitionArgument]  = arguments,
      body: Expression                     = body,
      location: Option[IdentifiedLocation] = location,
      canBeTCO: Boolean                    = canBeTCO,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Lambda = {
      if (
        arguments != this.arguments
        || body != this.body
        || location != this.location
        || canBeTCO != this.canBeTCO
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res =
          Lambda(
            arguments,
            Persistance.Reference.of(body, false),
            location.orNull,
            canBeTCO,
            passData
          )
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
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
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Lambda =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Lambda = {
      copy(arguments = arguments.map(_.mapExpressions(fn)), body = fn(body))
    }

    /** String representation. */
    override def toString: String =
      s"""
         |Function.Lambda(
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
      val bodyStr = if (body.isInstanceOf[Expression.Block]) {
        s"\n${body.showCode(indent)}"
      } else {
        s"${body.showCode(indent)}"
      }

      s"$args -> $bodyStr"
    }
  }

  object Lambda {

    def unapply(l: Lambda): Some[
      (
        List[DefinitionArgument],
        Expression,
        Option[IdentifiedLocation],
        Boolean,
        MetadataStorage,
        DiagnosticStorage
      )
    ] =
      Some(
        (
          l.arguments,
          l.body,
          l.location,
          l.canBeTCO,
          l.passData,
          l.diagnostics
        )
      )
  }

  /** A representation of the syntactic sugar for defining functions.
    *
    * @param name        the name of the function
    * @param arguments   the arguments to the function
    * @param body        the body of the function
    * @param isPrivate    Whether the function is project-private
    * @param location    the source location that the node corresponds to
    * @param canBeTCO    whether or not the function can be tail-call optimised
    * @param passData    the pass metadata associated with this node
    */
  sealed case class Binding(
    name: Name,
    override val arguments: List[DefinitionArgument],
    override val body: Expression,
    override val isPrivate: Boolean,
    override val identifiedLocation: IdentifiedLocation,
    override val canBeTCO: Boolean         = true,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Function
      with IRKind.Sugar
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param name        the name of the function
      * @param arguments   the arguments to the function
      * @param body        the body of the function
      * @param isPrivate    Whether the function is project-private
      * @param location    the source location that the node corresponds to
      * @param canBeTCO    whether or not the function can be tail-call optimised
      * @param passData    the pass metadata associated with this node
      * @param diagnostics the compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: Name                           = name,
      arguments: List[DefinitionArgument]  = arguments,
      body: Expression                     = body,
      isPrivate: Boolean                   = isPrivate,
      location: Option[IdentifiedLocation] = location,
      canBeTCO: Boolean                    = canBeTCO,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Binding = {
      if (
        name != this.name
        || arguments != this.arguments
        || body != this.body
        || isPrivate != this.isPrivate
        || location != this.location
        || canBeTCO != this.canBeTCO
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res =
          Binding(
            name,
            arguments,
            body,
            isPrivate,
            location.orNull,
            canBeTCO,
            passData
          )
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
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
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Binding =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Binding =
      copy(
        name      = name.mapExpressions(fn),
        arguments = arguments.map(_.mapExpressions(fn)),
        body      = fn(body)
      )

    /** String representation. */
    override def toString: String =
      s"""
         |Function.Binding(
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
      val bodyStr = if (body.isInstanceOf[Expression.Block]) {
        s"\n${body.showCode(indent)}"
      } else {
        s"${body.showCode(indent)}"
      }

      s"${name.name} $argsStr = $bodyStr"
    }
  }
}
