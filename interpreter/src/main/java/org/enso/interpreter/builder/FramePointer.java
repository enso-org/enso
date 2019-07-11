package org.enso.interpreter.builder;

import com.oracle.truffle.api.frame.FrameSlot;

public class FramePointer {
  private final int parentLevel;
  private final FrameSlot frameSlot;

  public FramePointer(int parentLevel, FrameSlot frameSlot) {
    this.parentLevel = parentLevel;
    this.frameSlot = frameSlot;
  }

  public int getParentLevel() {
    return parentLevel;
  }

  public FrameSlot getFrameSlot() {
    return frameSlot;
  }
}
