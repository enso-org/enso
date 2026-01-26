package org.enso.compiler.core.ir;

import scala.collection.immutable.AbstractSeq;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;

/** Storage for diagnostics in IR nodes. */
public final class DiagnosticStorage {
  private List<Diagnostic> diagnostics;

  @SuppressWarnings("unchecked")
  private DiagnosticStorage() {
    this((Seq<Diagnostic>) (Object) scala.collection.immutable.Nil$.MODULE$);
  }

  /**
   * @param initDiagnostics the initial diagnostics
   */
  public DiagnosticStorage(Seq<Diagnostic> initDiagnostics) {
    diagnostics = initDiagnostics.toList();
  }

  /**
   * Adds a new diagnostic to the storage
   *
   * @param diagnostic the new diagnostic to store
   */
  @SuppressWarnings("unchecked")
  public void add(Diagnostic diagnostic) {
    var next = ((AbstractSeq) diagnostics).prepended(diagnostic);
    diagnostics = (List<org.enso.compiler.core.ir.Diagnostic>) next;
  }

  /**
   * Adds new diagnostics to the storage.
   *
   * @param newDiagnostics the new diagnostics to store
   */
  public void add(Seq<Diagnostic> newDiagnostics) {
    var next = diagnostics.prependedAll(newDiagnostics);
    diagnostics = next;
  }

  /**
   * Checks two diagnostics storages for equality.
   *
   * @param obj the object to check against `this`
   * @return `true` if `this == obj`, otherwise `false`
   */
  public boolean equals(Object obj) {
    return obj instanceof DiagnosticStorage other ? diagnostics.equals(other.diagnostics) : false;
  }

  /**
   * Creates a string representation of `this` diagnostic storage.
   *
   * @return the string representation of `this`
   */
  public String toString() {
    return "DiagnosticStorage(diagnostics = " + diagnostics + ")";
  }

  /**
   * Creates a list of the diagnostics contained in the diagnostics storage.
   *
   * @return a list of the diagnostics in the storage
   */
  public List<Diagnostic> toList() {
    return diagnostics;
  }

  /**
   * Creates a shallow copy of `this`.
   *
   * <p>This means that the diagnostic objects contained in `this` and the copy are the same
   * objects.
   *
   * @return a shallow copy of this
   */
  public DiagnosticStorage copy() {
    return new DiagnosticStorage(this.diagnostics);
  }

  public static DiagnosticStorage createEmpty() {
    return new DiagnosticStorage();
  }
}
