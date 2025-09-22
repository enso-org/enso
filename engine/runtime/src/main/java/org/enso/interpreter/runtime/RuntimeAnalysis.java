package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.polyglot.RuntimeID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

public class RuntimeAnalysis {
  private final Stack<RuntimeID> idStack;
  private final Map<RuntimeID, Set<RuntimeID>> deps;
  private final static Logger LOGGER = LoggerFactory.getLogger(RuntimeAnalysis.class);

  public RuntimeAnalysis(EnsoContext ctx) {
    idStack = new Stack<>();
    deps = new HashMap<>();
  }

  public static RuntimeAnalysis create(EnsoContext ctx) {
    return new RuntimeAnalysis(ctx);
  }

  public void enterNode(RuntimeID id) {
    if (id != null) {
      var top = idStack.isEmpty() ? null : idStack.peek();
      if (top != null) {
        deps.computeIfAbsent(top, _ -> new HashSet<>()).add(id);
      }
      idStack.push(id);
    }
  }

  public void exitNode(RuntimeID id) {
    if (id != null) {
      if (idStack.isEmpty()) {
        LOGGER.warn("Should not attempt to exit empty stack for {}", id);
        return;
      }
      assert (idStack.peek() == id);
      idStack.pop();
    }
  }

  public RuntimeID peek() {
    if (idStack.isEmpty()) return null;
    else return idStack.peek();
  }

  @CompilerDirectives.TruffleBoundary
  public void registerLocalDependency(RuntimeID dependency) {
    if (dependency == null) {
      return;
    }
    var top = idStack.isEmpty() ? null : idStack.peek();
    if (top != null) {
      deps.computeIfAbsent(top, _ -> new HashSet<>()).add(dependency);
    } else {
      LOGGER.warn("Unable to register {} as a top-level local variable for runtime analysis", dependency);
    }
  }

  public void registerCallerCalleeDependency(RuntimeID dependency) {
    if (dependency == null) {
      return;
    }
    var top = idStack.isEmpty() ? null : idStack.peek();
    if (top != null) {
      deps.computeIfAbsent(top, _ -> new HashSet<>()).add(dependency);
    }
  }

  public Map<RuntimeID, Set<RuntimeID>> currentSnapshot() {
    return new HashMap<>(deps);
  }
}
