package org.enso.interpreter.builder;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlot;
import org.enso.interpreter.runtime.errors.VariableRedefinitionException;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class LocalScope {
  private Map<String, FrameSlot> items;
  private FrameDescriptor frameDescriptor;
  private LocalScope parent;

  public LocalScope() {
    items = new HashMap<>();
    frameDescriptor = new FrameDescriptor();
    parent = null;
  }

  public LocalScope(LocalScope parent) {
    this();
    this.parent = parent;
  }

  public FrameDescriptor getFrameDescriptor() {
    return frameDescriptor;
  }

  public LocalScope getParent() {
    return parent;
  }

  public LocalScope createChild() {
    return new LocalScope(this);
  }

  public FrameSlot createVarSlot(String name) {
    if (items.containsKey(name)) throw new VariableRedefinitionException(name);
    FrameSlot slot = frameDescriptor.addFrameSlot(name);
    items.put(name, slot);
    return slot;
  }

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
