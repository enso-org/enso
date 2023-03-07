package org.enso.base;

public class CompareException extends RuntimeException {
  private final Object leftOperand;
  private final Object rightOperand;

  public CompareException(Object leftOperand, Object rightOperand) {
    this.leftOperand = leftOperand;
    this.rightOperand = rightOperand;
  }

  public Object getLeftOperand() {
    return leftOperand;
  }

  public Object getRightOperand() {
    return rightOperand;
  }
}
