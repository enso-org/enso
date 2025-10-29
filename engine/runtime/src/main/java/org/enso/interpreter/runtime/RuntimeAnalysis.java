package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import org.enso.polyglot.RuntimeID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RuntimeAnalysis {
  private final Stack<RuntimeID> idStack;
  private final Map<RuntimeID, Set<RuntimeID>> deps;
  private RuntimeID closureEntry;
  private static final Logger LOGGER = LoggerFactory.getLogger(RuntimeAnalysis.class);

  public RuntimeAnalysis(EnsoContext ctx) {
    idStack = new Stack<>();
    deps = new HashMap<>();
    closureEntry = null;
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
      // assert (idStack.peek() == id);
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
      LOGGER.warn(
          "Unable to register {} as a top-level local variable for runtime analysis", dependency);
    }
  }

  public void registerCallerCalleeDependency(RuntimeID dependency) {
    if (dependency == null) {
      return;
    }
    var top = idStack.isEmpty() ? null : idStack.peek();
    if (top != null) {
      deps.computeIfAbsent(top, _ -> new HashSet<>()).add(dependency);
    } else {
      // assert closureEntry == null;
      closureEntry = dependency;
    }
  }

  public void registerCallableArg(RuntimeID argId, RuntimeID callableId) {
    if (argId != null && callableId != null) {
      deps.computeIfAbsent(argId, _ -> new HashSet<>()).add(callableId);
    }
  }

  public Map<RuntimeID, Set<RuntimeID>> currentSnapshot() {
    return new HashMap<>(deps);
  }

  public RuntimeID entryNode() {
    var v = closureEntry;
    closureEntry = null;
    return v;
  }

  public void reset() {
    deps.clear();
    idStack.clear();
    closureEntry = null;
  }
}
