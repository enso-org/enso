package org.enso.interpreter.instrument;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

/**
 * A storage for executed methods.
 *
 * <p>Tracks the encountered method calls during the invocation of the stack frame. Method calls
 * cache is not preserved between the program invocations.
 */
public final class MethodCallsCache {

  private final Set<UUID> callsExecuted = new HashSet<>();

  /** Save the executed method call. */
  public void setExecuted(UUID call) {
    callsExecuted.add(call);
  }

  /** @return the set of executed calls. */
  public Set<UUID> getCallsExecuted() {
    return callsExecuted;
  }

  /**
   * Get the subset of method calls that were not executed.
   *
   * @param calls the collection of all calls.
   * @return the set of calls that were not executed.
   */
  public Set<UUID> getNotExecuted(Collection<UUID> calls) {
    HashSet<UUID> cached = new HashSet<>(calls);
    cached.removeAll(callsExecuted);
    return cached;
  }
}
