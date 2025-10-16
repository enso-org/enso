package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Sheet;

/* A sheet reader for XLSB sheets. */
public final class XlsbExcelSheetReader implements ExcelSheetReader {

  private final int sheetIndex;
  private final String sheetName;
  private final XlsbExcelWorkbookReader.XlsbSheetContentsHandler handler;

  public XlsbExcelSheetReader(
      int sheetIndex, String sheetName, XlsbExcelWorkbookReader.XlsbSheetContentsHandler handler) {
    this.sheetIndex = sheetIndex;
    this.sheetName = sheetName;
    this.handler = handler;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public String getName() {
    return sheetName;
  }

  @Override
  public int getFirstRow() throws InterruptedException {
    return handler.getFirstRowNumber();
  }

  @Override
  public int getLastRow() throws InterruptedException {
    return handler.getLastRowNumber();
  }

  @Override
  public ExcelRow get(int row) throws InterruptedException {
    if (row <= 0) {
      return null;
    }

    var rowData = handler.getRowData(row);
    if (rowData == null) {
      return null;
    }

    return new XlsbExcelRow(rowData);
  }

  @Override
  public Sheet getSheet() {
    // Not supported as we don't have the underlying Apache POI Sheet object.
    throw new UnsupportedOperationException(
        "XLSBReader does not support getting the Sheet object.");
  }
}
