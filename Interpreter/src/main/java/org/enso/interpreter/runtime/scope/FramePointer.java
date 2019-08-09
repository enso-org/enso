package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.frame.FrameSlot;

/**
 * A representation of a pointer into a stack frame at a given number of levels above the current.
 */
public class FramePointer {
  private final int parentLevel;
  private final FrameSlot frameSlot;

  /**
   * A representation of a frame slot at a given level above the current frame.
   *
   * @param parentLevel the number of parents to move from the current frame to get here
   * @param frameSlot the slot in the n-th parent frame
   */
  public FramePointer(int parentLevel, FrameSlot frameSlot) {
    this.parentLevel = parentLevel;
    this.frameSlot = frameSlot;
  }

  /**
   * Gets the parent level.
   *
   * @return the parent level represented by this {@code FramePointer}
   */
  public int getParentLevel() {
    return parentLevel;
  }

  /**
   * Gets the frame slot.
   *
   * @return the frame slot represented by this {@code FramePointer}
   */
  public FrameSlot getFrameSlot() {
    return frameSlot;
  }
}
