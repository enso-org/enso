package org.enso.interpreter.builder;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlot;

import java.util.HashMap;
import java.util.Map;

public class LocalScope {
    private Map<String, FrameSlot> items;

  public FrameDescriptor getFrameDescriptor() {
    return frameDescriptor;
  }

  private FrameDescriptor frameDescriptor;

    public LocalScope getParent() {
      return parent;
    }

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

    public LocalScope createChild() {
      return new LocalScope(this);
    }

    public FrameSlot createVarSlot(String name) {
      if (items.containsKey(name)) throw new VariableRedefinitionException(name);
      FrameSlot slot = frameDescriptor.addFrameSlot(name);
      items.put(name, slot);
      return slot;
    }

    public FramePointer getSlot(String name) {
      LocalScope scope = this;
      int parentCounter = 0;
      while (scope != null) {
        FrameSlot slot = scope.items.get(name);
        if (slot != null) {
          return new FramePointer(parentCounter, slot);
        }
        scope = scope.parent;
        parentCounter++;
      }
      throw new VariableDoesNotExistException(name);
    }
  }
