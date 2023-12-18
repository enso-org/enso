package org.enso.table.error;

import org.enso.table.data.column.storage.type.StorageType;

/** Indicates that a value given to a builder did not fit its expected type. */
public class ValueTypeMismatchException extends RuntimeException {
  private final StorageType expectedType;
  private final Object value;

  public ValueTypeMismatchException(StorageType expectedType, Object value) {
    super(
        "Invalid value type. Expected "
            + expectedType
            + ", but got a value "
            + value
            + " ("
            + value.getClass().getSimpleName()
            + ") that does not fit it. If this error is ever encountered in user-facing code, that"
            + " is a bug in the Table library.");
    this.expectedType = expectedType;
    this.value = value;
  }

  public StorageType getExpectedType() {
    return expectedType;
  }

  public Object getValue() {
    return value;
  }
}
