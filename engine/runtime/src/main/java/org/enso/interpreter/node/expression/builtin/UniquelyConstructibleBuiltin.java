package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.List;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.atom.AtomNewInstanceNode;

public abstract class UniquelyConstructibleBuiltin extends Builtin {
  private @CompilerDirectives.CompilationFinal AtomConstructor uniqueConstructor;

  public final AtomConstructor getUniqueConstructor() {
    return uniqueConstructor;
  }

  @Override
  protected final List<Cons> getDeclaredConstructors() {
    return List.of(new Cons(getConstructorName(), getConstructorParamNames()));
  }

  protected String getConstructorName() {
    return getName() + "_Data";
  }

  protected abstract List<String> getConstructorParamNames();

  @Override
  protected void postInitialize() {
    uniqueConstructor = getConstructors()[0];
  }

  public final Atom newInstance(Object... params) {
    return AtomNewInstanceNode.getUncached().newInstance(uniqueConstructor, params);
  }
}
