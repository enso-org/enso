package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.UUID;

public class RuntimeAnalysis {
  private Stack<UUID> uuids;
  private Map<UUID, Set<UUID>> deps;

  public RuntimeAnalysis(EnsoContext ctx) {
    uuids = new Stack<>();
    deps = new HashMap<>();
  }

  public static RuntimeAnalysis create(EnsoContext ctx) {
    return new RuntimeAnalysis(ctx);
  }

  public void enterNode(UUID uuid) {
    if (uuid != null) {
      uuids.push(uuid);
    }
  }

  public void exitNode(UUID uuid) {
    if (uuid != null) {
      if (uuids.isEmpty()) {
        System.err.println("Should not attempt to exit empty stack. Investigate");
        return;
      }
      assert (uuids.peek() == uuid);
      uuids.pop();
    }
  }

  @CompilerDirectives.TruffleBoundary
  public void registerLocalDependency(UUID dependency) {
    var top = uuids.isEmpty() ? null : uuids.peek();
    if (top != null) {
      deps.computeIfAbsent(top, _ -> new HashSet<>()).add(dependency);
    } else {
      throw new RuntimeException(
          "Unable to register "
              + dependency
              + " as a top-level local variable for runtime analysis");
    }
  }

  public Map<UUID, Set<UUID>> currentSnapshot() {
    return new HashMap<>(deps);
  }
}
