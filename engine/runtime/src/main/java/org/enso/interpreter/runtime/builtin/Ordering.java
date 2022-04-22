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

  @CompilerDirectives.CompilationFinal
  private AtomConstructor ordering;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor less;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor equal;

  @CompilerDirectives.CompilationFinal
  private AtomConstructor greater;

  @CompilerDirectives.CompilationFinal
  private Builtins builtins;


  public Ordering(Builtins builtins) {
    this.builtins = builtins;
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
    if (ordering == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      ordering = builtins.getBuiltinType(org.enso.interpreter.node.expression.builtin.ordering.Ordering.class);
    }
    return ordering;
  }

  /** @return the Less constructor */
  public AtomConstructor less() {
    if (less == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      less = builtins.getBuiltinType(Less.class);
    }
    return less;
  }

  /** @return the Equal constructor */
  public AtomConstructor equal() {
    if (equal == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      equal = builtins.getBuiltinType(Equal.class);
    }
    return equal;
  }

  /** @return the Greater constructor */
  public AtomConstructor greater() {
    if (greater == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      greater = builtins.getBuiltinType(Greater.class);
    }
    return greater;
  }
}
