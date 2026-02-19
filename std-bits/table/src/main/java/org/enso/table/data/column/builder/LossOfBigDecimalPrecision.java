package org.enso.table.data.column.builder;

import java.math.BigDecimal;
import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

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

  @Override
  public Value asEnsoValue() {
    var textExampleValue = getExampleValue().toPlainString();
    var ensoDecimal =
        EnsoMeta.eval("Standard.Base.Data.Decimal", "Decimal.new '" + textExampleValue + "'");

    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Loss_Of_Decimal_Precision",
        "Warning",
        getAffectedRowsCount(),
        ensoDecimal,
        getExampleValueConverted());
  }
}
