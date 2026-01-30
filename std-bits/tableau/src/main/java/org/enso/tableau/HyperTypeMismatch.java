package org.enso.tableau;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

class HyperTypeMismatch extends RuntimeException {
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

  public Value asEnsoAtom() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Column_Type_Mismatch",
        "Error",
        getColumnName(),
        getExpectedType(),
        getActualType());
  }
}
