package org.enso.table.error;

import java.util.List;

public class NullValuesInKeyColumns extends RuntimeException {
  public final List<Object> exampleValues;

  public NullValuesInKeyColumns(List<Object> exampleValues) {
    this.exampleValues = exampleValues;
  }
}
