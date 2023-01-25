package org.enso.table.error;

public class EmptySheetException extends RuntimeException {
  public EmptySheetException() {
    super("Cannot parse an empty sheet.");
  }
}
