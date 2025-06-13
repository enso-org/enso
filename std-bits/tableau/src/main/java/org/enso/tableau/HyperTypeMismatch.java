package org.enso.tableau;

public class HyperTypeMismatch extends RuntimeException {
  private final String columnName;
  private final String expectedType;
  private final String actualType;

  public HyperTypeMismatch(String columnName, String expectedType, String actualType) {
    super(
        "Type mismatch found in column "
            + columnName
            + ": expected "
            + expectedType
            + ", actual "
            + actualType
            + ".");
    this.columnName = columnName;
    this.expectedType = expectedType;
    this.actualType = actualType;
  }

  public String getColumnName() {
    return columnName;
  }

  public String getExpectedType() {
    return expectedType;
  }

  public String getActualType() {
    return actualType;
  }
}
