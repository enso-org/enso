package org.enso.interpreter.runtime.data.text;

public class ConcatRope {
  private final Object left;
  private final Object right;

  public ConcatRope(Object left, Object right) {
    this.left = left;
    this.right = right;
  }

  public Object getLeft() {
    return left;
  }

  public Object getRight() {
    return right;
  }
}
