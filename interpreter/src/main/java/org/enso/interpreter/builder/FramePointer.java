package org.enso.interpreter.builder;

import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlot;

public class FramePointer {
  public int getParentLevel() {
    return parentLevel;
  }

  public FrameSlot getFrameSlot() {
    return frameSlot;
  }

  public FramePointer(int parentLevel, FrameSlot frameSlot) {
    this.parentLevel = parentLevel;
    this.frameSlot = frameSlot;
  }

  private final int parentLevel;
  private final FrameSlot frameSlot;
}

