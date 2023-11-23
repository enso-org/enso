package org.enso.table.error;

import java.util.List;

public class NonUniqueLookupKey extends RuntimeException {
  public final List<String> keyColumns;
  public final List<Object> exampleValues;
  public final int exampleCount;

  public NonUniqueLookupKey(List<String> keyColumns, List<Object> exampleValues, int exampleCount) {
    this.keyColumns = keyColumns;
    this.exampleValues = exampleValues;
    this.exampleCount = exampleCount;
  }
}
