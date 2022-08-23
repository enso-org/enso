package org.enso.interpreter.runtime.scope;

/**
 * A representation of a pointer into a stack frame at a given number of levels above the current.
 */
public class FramePointer {
  private final int parentLevel;
  private final int frameSlotIdx;

  /**
   * A representation of a frame slot at a given level above the current frame.
   *
   * @param parentLevel the number of parents to move from the current frame to get here
   * @param frameSlotIdx the index of the slot in the n-th parent frame
   */
  public FramePointer(int parentLevel, int frameSlotIdx) {
    this.parentLevel = parentLevel;
    this.frameSlotIdx = frameSlotIdx;
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
   * Gets the index of the frame slot.
   *
   * @return the frame slot index represented by this {@code FramePointer}
   */
  public int getFrameSlotIdx() {
    return frameSlotIdx;
  }
}
