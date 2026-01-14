package org.enso.interpreter.runtime.execution;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import java.util.UUID;
import org.enso.polyglot.RuntimeID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RuntimeAnalysis {
  private final Stack<Ref> assignmentsStack;
  private final Map<UUID, Ref> references = new HashMap<>(); // TODO: Make it a soft reference

  private static int COUNTER = 0;
  private final int id;
  private final int parentId;

  private final Logger LOGGER = LoggerFactory.getLogger(RuntimeAnalysis.class);

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

  private Ref getOrCreateReference(RuntimeID runtimeID) {
    var ref = references.get(runtimeID.uuid());
    if (ref == null) {
      ref = new RefObject(runtimeID);
      references.put(runtimeID.uuid(), ref);
    }
    return ref;
  }

  public void startRhsExecution(RuntimeID rhsId, String description) {
    var ref = getOrCreateReference(rhsId);
    assignmentsStack.push(ref);
  }

  public Ref currentRhs(String description) {
    if (assignmentsStack.isEmpty()) {
      return null;
    }
    return assignmentsStack.peek();
  }

  public void endRhsExecution(RuntimeID runtimeID, String description) {
    if (assignmentsStack.isEmpty()) {
      LOGGER.warn(
          "Empty runtime assignments stack encountered in {} @ {}", description, this.getId());
    } else {

      var popped = assignmentsStack.pop();
      if (!runtimeID.equals(popped.getRuntimeID())) {
        LOGGER.debug(
            "Unexpected expression ID popped from the stack. Expected {}, got {} in {} @ {}",
            runtimeID,
            popped.getRuntimeID(),
            description,
            this.id);
      }
    }
  }

  public Ref get(RuntimeID runtimeID) {
    return references.get(runtimeID.uuid());
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
