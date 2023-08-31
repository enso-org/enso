package org.enso.compiler.core.ir

import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{randomId, Identifier, ToStringHelper}

/** Constructs that operate on types. */
trait Type extends Expression {

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
  trait Info {
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
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
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
      s"""Type.Function(
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
    * @param typed       the expression being ascribed a type
    * @param signature   the signature being ascribed to `typed`
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Ascription(
    typed: Expression,
    signature: Expression,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Type
      with module.scope.Definition
      with IRKind.Primitive {
    override protected var id: Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param typed       the expression being ascribed a type
      * @param signature   the signature being ascribed to `typed`
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
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
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
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
      s"""Type.Ascription(
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
    * @param typed       the type being ascribed a monadic context
    * @param context     the context being ascribed to `typed`
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
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
      * @param typed       the type being ascribed a monadic context
      * @param context     the context being ascribed to `typed`
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
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
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
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
      s"""Type.Context(
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
    * @param typed       the expression being ascribed an error context
    * @param error       the error being ascribed
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
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
      * @param typed       the expression being ascribed an error context
      * @param error       the error being ascribed
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
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
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
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
      s"""Type.Error(
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

}
