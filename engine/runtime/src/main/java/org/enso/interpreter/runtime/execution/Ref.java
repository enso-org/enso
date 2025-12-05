package org.enso.interpreter.runtime.execution;

import java.util.stream.Stream;
import org.enso.polyglot.RuntimeID;

public abstract sealed class Ref permits RefObject {
  private final RuntimeID runtimeID;

  public Ref(RuntimeID runtimeID) {
    this.runtimeID = runtimeID;
  }

  public RuntimeID getRuntimeID() {
    return runtimeID;
  }

  public abstract Stream<Ref> reset();

  public abstract void update(Object value);

  public abstract void registerDependency(Ref dep);

  public abstract Object get();
}
