package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.node.expression.builtin.system.*;
import org.enso.interpreter.runtime.callable.atom.Atom;

/** A container class for all System-related stdlib builtins. */
public class System {

  private final BuiltinAtomConstructor systemProcessResult;

  /** Create builders for all {@code System} atom constructors. */
  public System(Builtins builtins) {
    systemProcessResult = new BuiltinAtomConstructor(builtins, SystemProcessResult.class);
  }

  /** @return the atom constructor for {@code Process_Result}. */
  public Atom makeSystemResult(Object exitCode, Object stdout, Object stderr) {
    return systemProcessResult.newInstance(exitCode, stdout, stderr);
  }
}
