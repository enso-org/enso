package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.Type;

import static com.oracle.truffle.api.CompilerDirectives.transferToInterpreterAndInvalidate;

public class BuiltinType {
  private final Builtins builtins;
  private final Class<? extends Builtin> clazz;

  @CompilerDirectives.CompilationFinal Type type;

  public BuiltinType(Builtins builtins, Class<? extends Builtin> clazz) {
    this.builtins = builtins;
    this.clazz = clazz;
  }

  public Type getType() {
    if (type == null) {
      transferToInterpreterAndInvalidate();
      type = builtins.getBuiltinType(clazz).getType();
    }
    return type;
  }

  //  Atom newInstance(Object... args) {
  //    return getType().newInstance(args);
  //  }
}
