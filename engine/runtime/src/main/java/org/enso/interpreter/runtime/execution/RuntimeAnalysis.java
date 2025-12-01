package org.enso.interpreter.runtime.execution;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.polyglot.RuntimeID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RuntimeAnalysis {
  private final EnsoContext ctx;
  private final Stack<Ref> assignmentsStack;
  private final Map<RuntimeID, Ref> references = new HashMap<>(); // Make it a soft reference

  private Logger LOGGER = LoggerFactory.getLogger(RuntimeAnalysis.class);

  private RuntimeAnalysis(EnsoContext ctx) {
    this.ctx = ctx;
    this.assignmentsStack = new Stack<>();
  }

  public static RuntimeAnalysis create(EnsoContext ctx) {
    return new RuntimeAnalysis(ctx);
  }

  private Ref getOrCreateReference(RuntimeID key) {
    var ref = references.get(key);
    if (ref == null) {
      ref = new RefObject(key);
      references.put(key, ref);
    }
    return ref;
  }

  public Ref startExecutingCachedExpression(RuntimeID runtimeID) {
    var ref = getOrCreateReference(runtimeID);
    assignmentsStack.push(ref);
    return ref;
  }

  public Ref currentlyExecutingExpression() {
    if (assignmentsStack.isEmpty()) {
      LOGGER.warn("Attempted to retrieve an empty assignment stack");
      return null;
    }

    return assignmentsStack.peek();
  }

  public void endExecutingCachedExpression(RuntimeID runtimeID) {
    if (assignmentsStack.isEmpty()) {
      LOGGER.debug("Empty runtime assignments stack");
    } else {
      var popped = assignmentsStack.pop();
      if (runtimeID != popped.getRuntimeID()) {
        LOGGER.warn(
            "Unexpected expression ID popped from the stack. Expected {}, got {}",
            runtimeID,
            popped.getRuntimeID());
      }
    }
  }
}
