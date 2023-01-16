package org.enso.table.data.column.builder.object;

public class StorageTypeMismatch extends RuntimeException {
  private final int expectedType;
  private final int gotType;

  public StorageTypeMismatch(int expectedType, int gotType) {
    this.expectedType = expectedType;
    this.gotType = gotType;
  }

  @Override
  public String getMessage() {
    return "Expected storage of type "
        + expectedType
        + ", got "
        + gotType
        + ". This is a bug in the Table library.";
  }

  public int gotType() {
    return gotType;
  }
}
