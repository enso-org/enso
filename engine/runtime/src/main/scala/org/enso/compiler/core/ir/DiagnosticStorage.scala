package org.enso.compiler.core.ir

import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Diagnostic

/** Storage for diagnostics in IR nodes.
 *
 * @param initDiagnostics the initial diagnostics
 */
sealed class DiagnosticStorage(initDiagnostics: Seq[Diagnostic] = Seq()) {
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

  /** Applies the function `f` across the diagnostic storage, producing a
   * result sequence.
   *
   * @param f the function to apply
   * @tparam R the result type of `f`
   * @return the sequence that results from applying `f` over the storage
   */
  def map[R](f: IR.Diagnostic => R): Seq[R] = {
    diagnostics.map(f)
  }

  /** Applies the function `f` across the diagnostic storage in place.
   *
   * @param f the function to apply
   */
  def mapInPlace(f: IR.Diagnostic => IR.Diagnostic): Unit = {
    diagnostics = diagnostics.map(f)
  }

  /** Performs a collection operation on the diagnostics storage, producing
   * a new sequence.
   *
   * @param pf the partial function to apply
   * @tparam R the result type of the partial function
   * @return the result of collecting across the storage with `pf`
   */
  def collect[R](pf: PartialFunction[IR.Diagnostic, R]): Seq[R] = {
    diagnostics.collect(pf)
  }

  /** Filters the elements of the diagnostic storage using the predicate.
   *
   * @param pred the predicate to filter with
   * @return a new diagnostic storage instance containing elements matching
   *         `pred`
   */
  def filter(pred: IR.Diagnostic => Boolean): DiagnosticStorage = {
    new DiagnosticStorage(diagnostics.filter(pred))
  }

  /** Filters the elements of the diagnostic storage in place using the
   * predicate.
   *
   * @param pred the predicate to filter with
   */
  def filterInPlace(pred: IR.Diagnostic => Boolean): Unit = {
    diagnostics = diagnostics.filter(pred)
  }

  /** Performs a left fold over the diagnostic storage to produce a result.
   *
   * @param init the starting value
   * @param op the operator to use to fold
   * @tparam L the result type of the fold
   * @return the result of folding over the storage using `op` starting wit
   *         `init`
   */
  def foldLeft[L](init: L)(op: (L, IR.Diagnostic) => L): L = {
    diagnostics.foldLeft(init)(op)
  }

  /** Checks two diagnostics storages for equality.
   *
   * @param obj the object to check against `this`
   * @return `true` if `this == obj`, otherwise `false`
   */
  override def equals(obj: Any): Boolean = obj match {
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
  def toList: List[IR.Diagnostic] = {
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
