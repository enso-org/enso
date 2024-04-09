package org.enso.table.error;

public class InvalidLocationException extends RuntimeException {
  public InvalidLocationException(String errorMessage) {
    super(errorMessage);
  }
}
