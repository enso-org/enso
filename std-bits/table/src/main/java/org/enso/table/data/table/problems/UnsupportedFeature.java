package org.enso.table.data.table.problems;

import org.enso.table.problems.Problem;

public class UnsupportedFeature implements Problem {
  private final String message;

  public UnsupportedFeature(String message) {
    this.message = message;
  }

  @Override
  public String toString() {
    return message;
  }

  public String getMessage() {
    return message;
  }
}
