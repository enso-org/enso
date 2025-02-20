package org.enso.table.data.column.builder;

import java.math.BigDecimal;
import org.enso.table.problems.Problem;

/** Indicates that a BigDecimal being converted to double cannot be represented precisely. */
public class LossOfBigDecimalPrecision implements Problem {
  private final BigDecimal exampleValue;
  private final double exampleValueConverted;
  private long affectedRows;

  public LossOfBigDecimalPrecision(BigDecimal exampleValue, double exampleValueConverted) {
    this.exampleValue = exampleValue;
    this.exampleValueConverted = exampleValueConverted;
    this.affectedRows = 1;
  }

  public BigDecimal getExampleValue() {
    return exampleValue;
  }

  public double getExampleValueConverted() {
    return exampleValueConverted;
  }

  public long getAffectedRowsCount() {
    return affectedRows;
  }

  void incrementAffectedRows() {
    affectedRows++;
  }
}
