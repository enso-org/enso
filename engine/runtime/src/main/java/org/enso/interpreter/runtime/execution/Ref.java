package org.enso.interpreter.runtime.execution;

import java.util.ArrayList;
import java.util.List;
import org.enso.polyglot.RuntimeID;

public abstract sealed class Ref permits RefObject {
  private final RuntimeID runtimeID;
  private final List<Ref> deps;

  public Ref(RuntimeID runtimeID) {
    this.runtimeID = runtimeID;
    deps = new ArrayList<>();
  }

  public RuntimeID getRuntimeID() {
    return runtimeID;
  }

  public abstract void reset();

  public abstract void update(Object value);

  public void registerDependency(Ref dep) {
    deps.add(dep);
  }

  public abstract boolean hasValue();

  public abstract Object get();
}
