package org.enso.interpreter.runtime.execution;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import org.enso.polyglot.RuntimeID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RuntimeAnalysis {
  private final Stack<Ref> assignmentsStack;
  private final Map<RuntimeID, Ref> references = new HashMap<>(); // Make it a soft reference

  private static int COUNTER = 0;
  private final int id;
  private final int parentId;

  private Logger LOGGER = LoggerFactory.getLogger(RuntimeAnalysis.class);

  private RuntimeAnalysis(int parentId) {
    this.id = COUNTER++;
    this.assignmentsStack = new Stack<>();
    this.parentId = parentId;
  }

  public static RuntimeAnalysis create() {
    return new RuntimeAnalysis(-1);
  }

  public static RuntimeAnalysis create(RuntimeAnalysis parent) {
    if (parent != null) {
      return new RuntimeAnalysis(parent.getId());
    } else {
      return RuntimeAnalysis.create();
    }
  }

  private Ref getOrCreateReference(RuntimeID cachedID) {
    var ref = references.get(cachedID);
    if (ref == null) {
      ref = new RefObject(cachedID);
      references.put(cachedID, ref);
    }
    return ref;
  }

  public Ref startRhsExecution(RuntimeID rhsId, String explanation) {
    var ref = getOrCreateReference(rhsId);
    assignmentsStack.push(ref);
    return ref;
  }

  public Ref currentRhs(String explanation) {
    if (assignmentsStack.isEmpty()) {
      return null;
    }
    return assignmentsStack.peek();
  }

  public void endRhsExecution(RuntimeID runtimeID, String explanation) {
    if (assignmentsStack.isEmpty()) {
      LOGGER.warn("Empty runtime assignments stack @ {}", this.getId());
    } else {

      var popped = assignmentsStack.pop();
      if (!runtimeID.equals(popped.getRuntimeID())) {
        LOGGER.warn(
            "Unexpected expression ID popped from the stack. Expected {}, got {} in {} @ {}",
            runtimeID,
            popped.getRuntimeID(),
            explanation,
            this.id);
      }
    }
  }

  public Ref get(RuntimeID runtimeID) {
    return references.get(runtimeID);
  }

  public void merge(RuntimeAnalysis analysis) {
    analysis.references.forEach(
        (key, value) -> {
          var existing = this.references.get(key);
          if (existing != null) {
            existing.merge(value);
          }
        });
  }

  public int getId() {
    return id;
  }

  @Override
  public String toString() {
    return "RuntimeAnalysis[id: "
        + id
        + ", parent: "
        + parentId
        + ", keys: "
        + references.keySet()
        + "]";
  }
}
