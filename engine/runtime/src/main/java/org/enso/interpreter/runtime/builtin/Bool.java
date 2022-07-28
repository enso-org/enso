package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.node.expression.builtin.Boolean;
import org.enso.interpreter.node.expression.builtin.bool.*;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;

/** A container class for all Boolean-related stdlib builtins. */
public class Bool {
  private final BuiltinType tru;
  private final BuiltinType fls;
  private final BuiltinType bool;

  /** Creates builders for all the boolean constructors. */
  public Bool(Builtins builtins) {
    bool = new BuiltinType(builtins, Boolean.class);
    tru = new BuiltinType(builtins, True.class);
    fls = new BuiltinType(builtins, False.class);
  }

  /** @return the atom constructor for {@code True}. */
  public AtomConstructor getTrue() {
    return tru.getType();
  }

  /** @return the atom constructor for {@code False}. */
  public AtomConstructor getFalse() {
    return fls.getType();
  }

  /** @return the atom constructor for {@code Boolean}. */
  public Type getBool() {
    return bool.getType();
  }
}
