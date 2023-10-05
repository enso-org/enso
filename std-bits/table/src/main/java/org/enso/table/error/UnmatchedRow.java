package org.enso.table.error;

import java.util.List;

public class UnmatchedRow extends RuntimeException {
  public final List<Object> exampleKeyValues;

  public UnmatchedRow(List<Object> exampleKeyValues) {
    this.exampleKeyValues = exampleKeyValues;
  }
}
