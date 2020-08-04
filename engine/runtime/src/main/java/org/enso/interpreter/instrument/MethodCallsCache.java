package org.enso.interpreter.instrument;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

/**
 * A storage for computed method calls. Method calls cache is not preserved between the program
 * invocations.
 */
public class MethodCallsCache {

  private final Set<UUID> callsExecuted = new HashSet<>();

  public void setExecuted(UUID call) {
    callsExecuted.add(call);
  }

  public Set<UUID> getNotExecuted(Collection<UUID> calls) {
    HashSet<UUID> cached = new HashSet<>(calls);
    cached.removeAll(callsExecuted);
    return cached;
  }
}
