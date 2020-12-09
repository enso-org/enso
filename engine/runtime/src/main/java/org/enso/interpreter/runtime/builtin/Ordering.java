package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container for builtin ordering types. */
public class Ordering {
  private final AtomConstructor less;
  private final AtomConstructor equal;
  private final AtomConstructor greater;

  public Ordering(Language language, ModuleScope scope) {
    less = new AtomConstructor("Less", scope).initializeFields();
    equal = new AtomConstructor("Equal", scope).initializeFields();
    greater = new AtomConstructor("Greater", scope).initializeFields();
    scope.registerConstructor(less);
    scope.registerConstructor(equal);
    scope.registerConstructor(greater);
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
