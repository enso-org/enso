package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

/** Wrapper class to handle Excel sheets. */
public class ExcelSheet {
  private final Sheet sheet;
  private final int firstRow;
  private final int lastRow;

  public ExcelSheet(Workbook workbook, int sheetIndex) {
    this.sheet = workbook.getSheetAt(sheetIndex);
    this.firstRow = sheet.getFirstRowNum() + 1;
    this.lastRow = sheet.getLastRowNum() + 1;
  }

  public int getLastRow() {
    return lastRow;
  }

  public int getFirstRow() {
    return firstRow;
  }

  public ExcelRow get(int row) {
    Row underlyingRow = row < firstRow || row > lastRow ? null : sheet.getRow(row - 1);
    return underlyingRow == null ? null : new ExcelRow(underlyingRow);
  }
}
