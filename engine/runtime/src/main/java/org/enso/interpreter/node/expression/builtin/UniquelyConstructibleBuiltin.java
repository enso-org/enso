package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

import java.util.List;

public abstract class UniquelyConstructibleBuiltin extends Builtin {
  private @CompilerDirectives.CompilationFinal AtomConstructor uniqueConstructor;

  public final AtomConstructor getUniqueConstructor() {
    return uniqueConstructor;
  }

  @Override
  protected final List<Cons> getDeclaredConstructors() {
    return List.of(new Cons(getName() + "_Data", getConstructorParamNames()));
  }

  protected abstract List<String> getConstructorParamNames();

  @Override
  protected void postInitialize() {
    uniqueConstructor = getConstructors()[0];
  }

  public final Atom newInstance(Object... params) {
    return uniqueConstructor.newInstance(params);
  }
}
