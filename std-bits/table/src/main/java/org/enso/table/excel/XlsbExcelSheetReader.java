package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Sheet;

/**
 * Skeleton sheet reader for XLSB sheets.
 *
 * <p>The implementation will be filled in when XLSB support is added.
 */
public final class XlsbExcelSheetReader implements ExcelSheetReader {

  private final int sheetIndex;
  private final String sheetName;

  public XlsbExcelSheetReader(int sheetIndex, String sheetName) {
    this.sheetIndex = sheetIndex;
    this.sheetName = sheetName;
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
    throw notImplemented();
  }

  @Override
  public int getLastRow() throws InterruptedException {
    throw notImplemented();
  }

  @Override
  public ExcelRow get(int row) throws InterruptedException {
    throw notImplemented();
  }

  @Override
  public Sheet getSheet() {
    throw notImplemented();
  }

  private UnsupportedOperationException notImplemented() {
    return new UnsupportedOperationException(
        "XLSB sheet support is not implemented yet for sheet " + sheetName);
  }
}
