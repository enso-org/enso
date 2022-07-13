package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.node.expression.builtin.ordering.Equal;
import org.enso.interpreter.node.expression.builtin.ordering.Greater;
import org.enso.interpreter.node.expression.builtin.ordering.Less;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** A container for builtin ordering types. */
public class Ordering {

  private final BuiltinType ordering;
  private final BuiltinType less;
  private final BuiltinType equal;
  private final BuiltinType greater;

  public Ordering(Builtins builtins) {
    ordering =
        new BuiltinType(
            builtins, org.enso.interpreter.node.expression.builtin.ordering.Ordering.class);
    less = new BuiltinType(builtins, Less.class);
    equal = new BuiltinType(builtins, Equal.class);
    greater = new BuiltinType(builtins, Greater.class);
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
    return ordering.getType();
  }

  /** @return the Less constructor */
  public AtomConstructor less() {
    return less.getType();
  }

  /** @return the Equal constructor */
  public AtomConstructor equal() {
    return equal.getType();
  }

  /** @return the Greater constructor */
  public AtomConstructor greater() {
    return greater.getType();
  }
}
