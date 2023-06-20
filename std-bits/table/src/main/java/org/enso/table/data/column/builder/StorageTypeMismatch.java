package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.type.StorageType;

public class StorageTypeMismatch extends RuntimeException {
  private final StorageType expectedType;
  private final StorageType gotType;

  public StorageTypeMismatch(StorageType expectedType, StorageType gotType) {
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

  public StorageType gotType() {
    return gotType;
  }
}
