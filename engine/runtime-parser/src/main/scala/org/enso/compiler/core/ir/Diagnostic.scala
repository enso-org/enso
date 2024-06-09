package org.enso.compiler.core.ir

/** A representation of various kinds of diagnostic in the IR. */
trait Diagnostic extends Serializable {

  /** @param source Location of the diagnostic.
    * @return a human-readable description of this error condition.
    */
  def message(source: (IdentifiedLocation => String)): String

  /** @param source Location of the diagnostic.
    * @return a human-readable description of this error condition, formatted for immediate reporting.
    */
  def formattedMessage(source: (IdentifiedLocation => String)): String =
    message(source)

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
    trait Static extends Kind

    /** Diagnostics that should remain at runtime for display during
      * interactive execution.
      */
    trait Interactive extends Kind
  }
}
