package org.enso.interpreter.runtime.execution;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.enso.polyglot.RuntimeID;

public final class RefObject extends Ref implements TruffleObject {
  private final Set<Ref> deps;
  private volatile Object value;

  public RefObject(RuntimeID runtimeID) {
    super(runtimeID);
    deps = new LinkedHashSet<>();
  }

  @Override
  public Set<Ref> dependencies() {
    return deps;
  }

  @Override
  public Object get() {
    return value;
  }

  @Override
  public void update(Object value) {
    this.value = value;
  }

  @Override
  public Stream<Ref> reset() {
    value = null;
    return deps.stream();
  }

  @Override
  public void registerDependency(Ref dep) {
    assert (!this.runtimeID.equals(dep.getRuntimeID())); // cyclic dependencies are not allowed
    this.deps.add(dep);
  }

  @Override
  public String toString() {
    return "Ref[runtimeID="
        + getRuntimeID()
        + ", value="
        + (value != null ? "<non-empty>" : "<empty>")
        + ", deps="
        + deps.stream().map(Ref::getRuntimeID).collect(Collectors.toSet())
        + "]";
  }
}
