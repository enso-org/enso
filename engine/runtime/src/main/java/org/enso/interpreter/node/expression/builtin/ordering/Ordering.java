package org.enso.interpreter.node.expression.builtin.ordering;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

@BuiltinType
public class Ordering extends Builtin {
  @Override
  protected List<Cons> getDeclaredConstructors() {
    return List.of(new Cons("Less"), new Cons("Equal"), new Cons("Greater"));
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

  /**
   * @return the Less constructor
   */
  public AtomConstructor less() {
    return getConstructors()[0];
  }

  /**
   * @return the Equal constructor
   */
  public AtomConstructor equal() {
    return getConstructors()[1];
  }

  /**
   * @return the Greater constructor
   */
  public AtomConstructor greater() {
    return getConstructors()[2];
  }

  /**
   * @return a new instance of Less
   */
  public Atom newLess() {
    return less().newInstance();
  }

  /**
   * @return a new instance of Equal
   */
  public Atom newEqual() {
    return equal().newInstance();
  }

  /**
   * @return a new instance of Greater
   */
  public Atom newGreater() {
    return greater().newInstance();
  }
}
