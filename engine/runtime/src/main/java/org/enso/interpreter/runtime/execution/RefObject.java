package org.enso.interpreter.runtime.execution;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.enso.polyglot.RuntimeID;

public final class RefObject extends Ref implements TruffleObject {
  private Object value;
  private final List<Ref> deps;

  public RefObject(RuntimeID runtimeID) {
    super(runtimeID);
    deps = new ArrayList<>();
  }

  public Object get() {
    return value;
  }

  @Override
  public Stream<Ref> reset() {
    value = null;
    return deps.stream();
  }

  @Override
  public void update(Object value) {
    this.value = value;
  }

  @Override
  public void registerDependency(Ref dep) {
    this.deps.add(dep);
  }

  @Override
  public String toString() {
    return "Ref[runtimeID="
        + getRuntimeID()
        + ", hasValue="
        + (value != null)
        + ", deps="
        + deps.size()
        + "]";
  }
}
