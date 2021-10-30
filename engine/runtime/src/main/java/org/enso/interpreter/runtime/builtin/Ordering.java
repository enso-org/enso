package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container for builtin ordering types. */
public class Ordering {
  private final AtomConstructor ordering;
  private final AtomConstructor less;
  private final AtomConstructor equal;
  private final AtomConstructor greater;

  public Ordering(Language language, ModuleScope scope) {
    ordering = new AtomConstructor("Ordering", scope).initializeFields();
    less = new AtomConstructor("Less", scope).initializeFields();
    equal = new AtomConstructor("Equal", scope).initializeFields();
    greater = new AtomConstructor("Greater", scope).initializeFields();
    scope.registerConstructor(ordering);
    scope.registerConstructor(less);
    scope.registerConstructor(equal);
    scope.registerConstructor(greater);
  }

  /**
   * Convert the java notion of ordering to the Enso notion of ordering.
   *
   * @param ord the java ordering
   * @return the Enso ordering corresponding to {@code ord}
   */
  public Atom fromJava(int ord) {
    if (ord == 0) {
      return newEqual();
    } else if (ord > 0) {
      return newGreater();
    } else {
      return newLess();
    }
  }

  /** @return a new instance of Less */
  public Atom newLess() {
    return less.newInstance();
  }

  /** @return a new instance of Equal */
  public Atom newEqual() {
    return equal.newInstance();
  }

  /** @return a new instance of Greater */
  public Atom newGreater() {
    return greater.newInstance();
  }

  /** @return the Ordering constructor. */
  public AtomConstructor ordering() {
    return ordering;
  }

  /** @return the Less constructor */
  public AtomConstructor less() {
    return less;
  }

  /** @return the Equal constructor */
  public AtomConstructor equal() {
    return equal;
  }

  /** @return the Greater constructor */
  public AtomConstructor greater() {
    return greater;
  }
}
