package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;

import java.util.function.IntFunction;

/** Wrapper class to handle Excel sheets. */
public class ExcelSheet {
  private final int firstRow;
  private final int lastRow;

  // Still to re-work
  private final IntFunction<Row> rowSupplier;
  private Sheet sheet;

  public ExcelSheet(int firstRow, int lastRow, IntFunction<Row> rowSupplier, Sheet sheet) {
    this.firstRow = firstRow;
    this.lastRow = lastRow;
    this.rowSupplier = rowSupplier;
  }

  public int getLastRow() {
    return lastRow;
  }

  public int getFirstRow() {
    return firstRow;
  }

  public ExcelRow get(int row) {
    Row underlyingRow = row < firstRow || row > lastRow ? null : rowSupplier.apply(row - 1);
    return underlyingRow == null ? null : new ExcelRow(underlyingRow);
  }

  public Sheet getSheet() {
    return sheet;
  }
}
