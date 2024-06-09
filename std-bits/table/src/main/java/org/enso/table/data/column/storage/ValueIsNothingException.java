package org.enso.table.data.column.storage;

public class ValueIsNothingException extends RuntimeException {
  public final long index;

  public ValueIsNothingException(long index) {
    this.index = index;
  }
}
