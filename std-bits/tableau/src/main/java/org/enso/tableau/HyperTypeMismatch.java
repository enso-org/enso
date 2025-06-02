package org.enso.tableau;

import java.util.List;

public class HyperTypeMismatch extends RuntimeException {
  public record Mismatch(String columnName, String expectedType, String actualType) {}

  private final List<Mismatch> mismatches;

  public HyperTypeMismatch(List<Mismatch> mismatches) {
    super("Type mismatches found in " + mismatches.size() + " column(s).");
    this.mismatches = mismatches;
  }

  public List<Mismatch> getMismatches() {
    return mismatches;
  }
}
