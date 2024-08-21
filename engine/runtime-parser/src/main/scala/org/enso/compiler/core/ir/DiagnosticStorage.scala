package org.enso.compiler.core.ir

/** Storage for diagnostics in IR nodes.
  *
  * @param initDiagnostics the initial diagnostics
  */
final class DiagnosticStorage(initDiagnostics: Seq[Diagnostic] = Seq())
    extends Serializable {
  private var diagnostics: List[Diagnostic] = initDiagnostics.toList

  /** Adds a new diagnostic to the storage
    *
    * @param diagnostic the new diagnostic to store
    */
  def add(diagnostic: Diagnostic): Unit = {
    diagnostics = diagnostic :: diagnostics
  }

  /** Adds new diagnostics to the storage.
    *
    * @param newDiagnostics the new diagnostics to store
    */
  def add(newDiagnostics: Seq[Diagnostic]): Unit = {
    diagnostics = newDiagnostics.toList ::: diagnostics
  }

  /** Checks two diagnostics storages for equality.
    *
    * @param obj the object to check against `this`
    * @return `true` if `this == obj`, otherwise `false`
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case that: DiagnosticStorage => this.diagnostics == that.diagnostics
      case _                       => false
    }

  /** Creates a string representation of `this` diagnostic storage.
    *
    * @return the string representation of `this`
    */
  override def toString: String =
    s"DiagnosticStorage(diagnostics = $diagnostics)"

  /** Creates a list of the diagnostics contained in the diagnostics storage.
    *
    * @return a list of the diagnostics in the storage
    */
  def toList: List[Diagnostic] = {
    diagnostics
  }

  /** Creates a shallow copy of `this`.
    *
    * This means that the diagnostic objects contained in `this` and the copy
    * are the same objects.
    *
    * @return a shallow copy of this
    */
  def copy: DiagnosticStorage = {
    DiagnosticStorage(this.diagnostics)
  }
}
object DiagnosticStorage {

  /** Creates a new instance of the diagnostics storage.
    *
    * @param initDiagnostics the initial diagnostics to construct it with
    * @return a new diagnostics storage instance
    */
  def apply(initDiagnostics: Seq[Diagnostic] = Seq()): DiagnosticStorage =
    new DiagnosticStorage(initDiagnostics)
}
