package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.system.*;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container class for all System-related stdlib builtins. */
public class System {

  private AtomConstructor system;
  private AtomConstructor systemProcessResult;
  private final Builtins builtins;

  /**
   * Create and register all {@code System} constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors and methods in.
   */
  public System(Builtins builtins) {
    this.builtins = builtins;
  }

  /** @return the atom constructor for {@code Process_Result}. */
  public Atom makeSystemResult(Object exitCode, Object stdout, Object stderr) {
    if (systemProcessResult == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      systemProcessResult = builtins.getBuiltinType(SystemProcessResult.class);
    }
    return systemProcessResult.newInstance(exitCode, stdout, stderr);
  }
}
