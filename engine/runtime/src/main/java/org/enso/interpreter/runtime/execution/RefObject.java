package org.enso.interpreter.runtime.execution;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.enso.polyglot.RuntimeID;

public final class RefObject extends Ref implements TruffleObject {
  private final Set<Ref> deps;

  public RefObject(RuntimeID runtimeID) {
    super(runtimeID);
    deps = new LinkedHashSet<>();
  }

  @Override
  public Set<Ref> dependencies() {
    return deps;
  }

  @Override
  public void merge(Ref ref) {
    assert ref.getRuntimeID().equals(this.runtimeID);
    deps.addAll(ref.dependencies());
  }

  @Override
  public Stream<Ref> reset() {
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
        + ", deps="
        + deps.stream().map(Ref::getRuntimeID).collect(Collectors.toSet())
        + "]";
  }
}
