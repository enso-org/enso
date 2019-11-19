package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import org.enso.interpreter.runtime.error.VariableRedefinitionException;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * A representation of an Enso local scope. These can be arbitrarily nested and are used to map
 * between the interpreter's concept of stack frames and the guest language's concept of stack
 * frames.
 */
public class LocalScope {
  private final FrameDescriptor frameDescriptor;
  private final LocalScope parent;
  private final Map<String, FrameSlot> items;
  private final FrameSlot stateFrameSlot;

  /** Creates a root local scope. */
  public LocalScope() {
    this(null);
  }

  /**
   * Creates a child local scope with a given parent.
   *
   * @param parent the parent scope
   */
  public LocalScope(LocalScope parent) {
    this.items = new HashMap<>();
    this.frameDescriptor = new FrameDescriptor();
    this.parent = parent;
    this.stateFrameSlot =
        frameDescriptor.findOrAddFrameSlot("<<monadic_state>>", FrameSlotKind.Object);
  }

  /**
   * Gets the frame descriptor for this scope.
   *
   * <p>A {@link FrameDescriptor} is a handle to an interpreter frame. This provides the means to
   * map between Enso's concept of frames, and the interpreter's concept of frames.
   *
   * @return the frame descriptor for this scope
   */
  public FrameDescriptor getFrameDescriptor() {
    return frameDescriptor;
  }

  /**
   * Gets the Enso-semantics parent of this scope.
   *
   * @return the parent scope
   */
  public LocalScope getParent() {
    return parent;
  }

  /**
   * Creates a scope that is the Enso-semantics child of this.
   *
   * @return a new scope with {@code this} as its parent
   */
  public LocalScope createChild() {
    return new LocalScope(this);
  }

  /**
   * Creates a new variable in the Enso frame.
   *
   * @param name the name of the variable
   * @return a handle to the defined variable
   */
  public FrameSlot createVarSlot(String name) {
    if (items.containsKey(name)) throw new VariableRedefinitionException(name);
    // The FrameSlot is created for a given identifier.
    FrameSlot slot = frameDescriptor.addFrameSlot(name);
    items.put(name, slot);
    return slot;
  }

  /**
   * Reads a variable from the Enso frame.
   *
   * @param name the name of the variable
   * @return a handle to the variable, otherwise {@link Optional#empty()}
   */
  public Optional<FramePointer> getSlot(String name) {
    LocalScope scope = this;
    int parentCounter = 0;
    while (scope != null) {
      FrameSlot slot = scope.items.get(name);
      if (slot != null) {
        return Optional.of(new FramePointer(parentCounter, slot));
      }
      scope = scope.parent;
      parentCounter++;
    }
    return Optional.empty();
  }

  /**
   * Gets the monadic state frame slot for this local scope.
   *
   * @return the frame slot containing monadic state
   */
  public FrameSlot getStateFrameSlot() {
    return stateFrameSlot;
  }

  private Map<String, FramePointer> flattenWithLevel(int level) {
    Map<String, FramePointer> parentResult =
        parent == null ? new HashMap<>() : getParent().flattenWithLevel(level + 1);
    for (Map.Entry<String, FrameSlot> entry : items.entrySet()) {
      parentResult.put(entry.getKey(), new FramePointer(level, entry.getValue()));
    }
    return parentResult;
  }

  /**
   * Returns a flat representation of the scope, including variables from all parent lexical scopes.
   *
   * @return a flat representation of this scope
   */
  public Map<String, FramePointer> flatten() {
    return flattenWithLevel(0);
  }
}
