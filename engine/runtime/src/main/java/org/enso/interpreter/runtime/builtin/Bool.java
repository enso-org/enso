package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.node.expression.builtin.Boolean;
import org.enso.interpreter.node.expression.builtin.bool.*;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** A container class for all Boolean-related stdlib builtins. */
public class Bool {
  private final BuiltinAtomConstructor tru;
  private final BuiltinAtomConstructor fls;
  private final BuiltinAtomConstructor bool;

  /** Creates builders for all the boolean constructors. */
  public Bool(Builtins builtins) {
    bool = new BuiltinAtomConstructor(builtins, Boolean.class);
    tru = new BuiltinAtomConstructor(builtins, True.class);
    fls = new BuiltinAtomConstructor(builtins, False.class);
  }

  /** @return the atom constructor for {@code True}. */
  public AtomConstructor getTrue() {
    return tru.constructor();
  }

  /** @return the atom constructor for {@code False}. */
  public AtomConstructor getFalse() {
    return fls.constructor();
  }

  /** @return the atom constructor for {@code Boolean}. */
  public AtomConstructor getBool() {
    return bool.constructor();
  }
}
