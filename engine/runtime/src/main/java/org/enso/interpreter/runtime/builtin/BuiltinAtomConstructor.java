package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

import static com.oracle.truffle.api.CompilerDirectives.transferToInterpreterAndInvalidate;

public class BuiltinAtomConstructor {
  private final Builtins builtins;
  private final Class<? extends Builtin> type;

  @CompilerDirectives.CompilationFinal AtomConstructor atom;

  public BuiltinAtomConstructor(Builtins builtins, Class<? extends Builtin> type) {
    this.builtins = builtins;
    this.type = type;
  }

  public AtomConstructor constructor() {
    if (atom == null) {
      transferToInterpreterAndInvalidate();
      atom = builtins.getBuiltinType(type);
    }
    return atom;
  }

  Atom newInstance(Object... args) {
    return constructor().newInstance(args);
  }
}
