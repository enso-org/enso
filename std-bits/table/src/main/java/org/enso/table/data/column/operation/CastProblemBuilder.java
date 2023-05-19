package org.enso.table.data.column.operation;

public class CastProblemBuilder {
  private int lossyConversionRowCount = 0;

  public void reportLossyConversion() {
    lossyConversionRowCount++;
  }

  public int getLossyConversionRowCount() {
    return lossyConversionRowCount;
  }
}
