package org.enso.interpreter.runtime.execution;

import java.util.Set;
import java.util.stream.Stream;
import org.enso.polyglot.RuntimeID;

/** Ref wraps a single value and allows for tracking dependencies between values * */
public abstract sealed class Ref permits RefObject {
  protected final RuntimeID runtimeID;

  public Ref(RuntimeID runtimeID) {
    this.runtimeID = runtimeID;
  }

  /** Returns an identifier of an expression that this reference wraps. */
  public RuntimeID getRuntimeID() {
    return runtimeID;
  }

  /**
   * Clears the value associated with this identifier and returns a stream of dependencies.
   *
   * @return a stream of references representing dependents of this reference
   */
  public abstract Stream<Ref> reset();

  /** Register a downstream dependency with this reference. */
  public abstract void registerDependency(Ref dep);

  /** Returns a list of downstream dependencies dependent on the value of this reference. */
  public abstract Set<Ref> dependencies();

  /** Returns the current underlying value for this reference. */
  public abstract Object get();

  /** Updates the underlying value of this reference. */
  public abstract void update(Object value);
}
