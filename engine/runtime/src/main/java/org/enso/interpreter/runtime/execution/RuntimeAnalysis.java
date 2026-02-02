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

  private final Logger LOGGER = LoggerFactory.getLogger(RuntimeAnalysis.class);

  private RuntimeAnalysis() {
    this.id = COUNTER++;
    this.assignmentsStack = new Stack<>();
  }

  public static RuntimeAnalysis create() {
    return new RuntimeAnalysis();
  }

  private Ref getOrCreateReference(RuntimeID runtimeID) {
    var ref = references.get(runtimeID.uuid());
    if (ref == null) {
      ref = new RefObject(runtimeID);
      references.put(runtimeID.uuid(), ref);
    }
    return ref;
  }

  public void startRhsExecution(RuntimeID rhsId) {
    var ref = getOrCreateReference(rhsId);
    assignmentsStack.push(ref);
  }

  public Ref currentRhs() {
    if (assignmentsStack.isEmpty()) {
      return null;
    }
    return assignmentsStack.peek();
  }

  public void endRhsExecution(RuntimeID runtimeID) {
    if (assignmentsStack.isEmpty()) {
      LOGGER.warn("Empty runtime assignments stack encountered @ {}", this.getId());
    } else {

      var popped = assignmentsStack.pop();
      if (!runtimeID.equals(popped.getRuntimeID())) {
        LOGGER.debug(
            "Unexpected expression ID popped from the stack. Expected {}, got {} @ {}",
            runtimeID,
            popped.getRuntimeID(),
            this.id);
      }
    }
  }

  public Ref get(RuntimeID runtimeID) {
    return references.get(runtimeID.uuid());
  }

  public int getId() {
    return id;
  }

  @Override
  public String toString() {
    return "RuntimeAnalysis[id: " + id + ", keys: " + references.keySet() + "]";
  }
}
