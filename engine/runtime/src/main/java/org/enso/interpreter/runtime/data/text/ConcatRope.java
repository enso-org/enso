package org.enso.interpreter.runtime.data.text;

/** Represents a concatenation of two text values. */
public class ConcatRope {
  private final Object left;
  private final Object right;

  /**
   * Creates a new rope concatenating the arguments.
   *
   * @param left the left operand
   * @param right the right operand
   */
  public ConcatRope(Object left, Object right) {
    this.left = left;
    this.right = right;
  }

  /**
   * @return the left operand of this concatenation.
   */
  public Object getLeft() {
    return left;
  }

  /**
   * @return the right operand of this concatenation.
   */
  public Object getRight() {
    return right;
  }
}
