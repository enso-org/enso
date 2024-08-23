package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

/** Wrapper class to handle Excel sheets. */
public interface ExcelSheet {
  /** Gets the index of the sheet within the workbook (0-based). */
  int getSheetIndex();

  /** Gets the name of the sheet. */
  String getName();

  /** Gets the initial row index within the sheet (1-based). */
  int getFirstRow();

  /** Gets the final row index within the sheet (1-based). */
  int getLastRow();

  /**
   * Gets the row at the given index within the sheet (1-based)
   *
   * @param row the row index (1-based)/
   * @return the row object or null if the row index is out of range or doesn't exist.
   */
  ExcelRow get(int row);

  /** Gets the underlying Apache POI Sheet object - may be null. Provided for Writer use only. */
  Sheet getSheet();

  /** Gets the underlying Apache POI Sheet object. */
  static ExcelSheet fromWorkbook(Workbook workbook, int sheetIndex) {
    var sheet = workbook.getSheetAt(sheetIndex);
    return new ExcelSheetFromWorkbook(
        sheet,
        sheetIndex,
        sheet.getSheetName(),
        sheet.getFirstRowNum() + 1,
        sheet.getLastRowNum() + 1);
  }

  record ExcelSheetFromWorkbook(
      Sheet sheet, int sheetIndex, String sheetName, int firstRow, int lastRow)
      implements ExcelSheet {
    @Override
    public int getSheetIndex() {
      return sheetIndex;
    }

    @Override
    public String getName() {
      return sheetName;
    }

    @Override
    public int getFirstRow() {
      return firstRow;
    }

    @Override
    public int getLastRow() {
      return lastRow;
    }

    @Override
    public ExcelRow get(int row) {
      return row < firstRow || row > lastRow ? null : ExcelRow.fromSheet(sheet, row);
    }

    @Override
    public Sheet getSheet() {
      return sheet;
    }
  }
}
