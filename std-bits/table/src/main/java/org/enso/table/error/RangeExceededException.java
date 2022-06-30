package org.enso.table.error;

public class RangeExceededException extends Exception {
  public RangeExceededException(String errorMessage) {
    super(errorMessage);
  }
}
