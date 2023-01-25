package org.enso.table.error;

public class EmptyFileException extends RuntimeException {
  public EmptyFileException() {
    super("Cannot parse an empty file.");
  }
}
