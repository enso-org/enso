package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.error.UninitializedState;
import org.enso.interpreter.node.expression.builtin.ordering.Equal;
import org.enso.interpreter.node.expression.builtin.ordering.Greater;
import org.enso.interpreter.node.expression.builtin.ordering.Less;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container for builtin ordering types. */
public class Ordering {

  private final BuiltinAtomConstructor ordering;
  private final BuiltinAtomConstructor less;
  private final BuiltinAtomConstructor equal;
  private final BuiltinAtomConstructor greater;

  public Ordering(Builtins builtins) {
    ordering =
        new BuiltinAtomConstructor(
            builtins, org.enso.interpreter.node.expression.builtin.ordering.Ordering.class);
    less = new BuiltinAtomConstructor(builtins, Less.class);
    equal = new BuiltinAtomConstructor(builtins, Equal.class);
    greater = new BuiltinAtomConstructor(builtins, Greater.class);
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
    return less().newInstance();
  }

  /** @return a new instance of Equal */
  public Atom newEqual() {
    return equal().newInstance();
  }

  /** @return a new instance of Greater */
  public Atom newGreater() {
    return greater().newInstance();
  }

  /** @return the Ordering constructor. */
  public AtomConstructor ordering() {
    return ordering.constructor();
  }

  /** @return the Less constructor */
  public AtomConstructor less() {
    return less.constructor();
  }

  /** @return the Equal constructor */
  public AtomConstructor equal() {
    return equal.constructor();
  }

  /** @return the Greater constructor */
  public AtomConstructor greater() {
    return greater.constructor();
  }
}
