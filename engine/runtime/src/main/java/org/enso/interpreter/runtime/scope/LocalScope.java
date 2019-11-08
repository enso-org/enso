package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlot;
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
  private Map<String, FrameSlot> items;
  private FrameDescriptor frameDescriptor;
  private LocalScope parent;

  /** Creates a new local scope with defaulted arguments. */
  public LocalScope() {
    items = new HashMap<>();
    frameDescriptor = new FrameDescriptor();
    parent = null;
  }

  /**
   * Creates a new local scope with a known parent.
   *
   * @param parent the parent scope
   */
  public LocalScope(LocalScope parent) {
    this();
    this.parent = parent;
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
}
