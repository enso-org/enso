package org.enso.compiler.core.ir
package expression
package errors

import org.enso.compiler.core.{IR, Identifier}

import java.util.UUID

/** A representation of an error resulting from name resolution.
  *
  * @param originalName the original name that could not be resolved
  * @param reason       the cause of this error
  * @param passData     the pass metadata associated with this node
  * @param diagnostics  compiler diagnostics for this node
  */
sealed case class Resolution(
  originalName: Name,
  reason: Resolution.Reason,
  passData: MetadataStorage      = new MetadataStorage(),
  diagnostics: DiagnosticStorage = DiagnosticStorage()
) extends Error
    with Diagnostic.Kind.Interactive
    with IRKind.Primitive
    with Name
    with LazyId {
  override val name: String = originalName.name

  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Resolution =
    this

  override def setLocation(
    location: Option[IdentifiedLocation]
  ): Resolution =
    copy(originalName = originalName.setLocation(location))

  /** Creates a copy of `this`.
    *
    * @param originalName the original name that could not be resolved
    * @param reason       the cause of this error
    * @param passData     the pass metadata associated with this node
    * @param diagnostics  compiler diagnostics for this node
    * @param id           the identifier for the new node
    * @return a copy of `this`, updated with the specified values
    */
  def copy(
    originalName: Name             = originalName,
    reason: Resolution.Reason      = reason,
    passData: MetadataStorage      = passData,
    diagnostics: DiagnosticStorage = diagnostics,
    id: UUID @Identifier           = id
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
        if (keepMetadata) passData.duplicate else new MetadataStorage(),
      diagnostics =
        if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
      id = if (keepIdentifiers) id else null
    )

  /** @inheritdoc */
  override def children: List[IR] = List(originalName)

  /** @inheritdoc */
  override def showCode(indent: Int): String = originalName.showCode(indent)

  /** @inheritdoc */
  override def message(source: (IdentifiedLocation => String)): String =
    reason.explain(originalName)

  /** @inheritdoc */
  override def formattedMessage(
    source: (IdentifiedLocation => String)
  ): String = s"${message(source)}."

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
      *
      * @param originalName the original unresolved name.
      * @return a human-readable message.
      */
    override def explain(originalName: Name): String =
      this.explain.explain(originalName)
  }

  trait ExplainResolution {
    def explain(originalName: Name): String
  }

  case class MissingLibraryImportInFQNError(namespace: String) extends Reason {
    override def explain(originalName: Name): String =
      s"Fully qualified name references a library $namespace.${originalName.name} but an import statement for it is missing"
  }

}
