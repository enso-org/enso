package org.enso.table.error;

public class ExistingDataException extends Exception {
  public ExistingDataException(String errorMessage) {
    super(errorMessage);
  }
}
