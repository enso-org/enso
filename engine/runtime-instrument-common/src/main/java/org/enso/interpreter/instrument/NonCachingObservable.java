package org.enso.interpreter.instrument;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.enso.polyglot.RuntimeID;

/**
 * Non-caching observables can record dependencies between observables but never store the
 * underlying value of the node. As such, no visualizations can ever be attached to such {@code
 * Observable}.
 */
public class NonCachingObservable implements Observable {
  private final RuntimeID id;
  private final Set<Observable> dependencies;

  public NonCachingObservable(RuntimeID id) {
    assert !id.isExternal();
    this.id = id;
    this.dependencies = new HashSet<>();
  }

  @Override
  public Observable register(Observable observable) {
    if (observable != null && observable != this) {
      synchronized (dependencies) {
        dependencies.add(observable);
      }
    }
    return this;
  }

  @Override
  public Stream<Observable> invalidate() {
    synchronized (dependencies) {
      return dependencies.stream();
    }
  }

  @Override
  public RuntimeID id() {
    return id;
  }

  @Override
  public Object get() {
    return null;
  }

  @Override
  public String toString() {
    var deps = dependencies.stream().map(dep -> dep.id()).collect(Collectors.toSet());
    return "Observable(id=" + id + ", direct dependencies=" + deps + ")";
  }
}
