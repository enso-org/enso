package org.enso.table.excel;

import java.io.IOException;

/** Represents an Excel workbook. Wraps the underlying Apache POI Workbook object. */
public interface ExcelWorkbookReader {
  /**
   * Get the number of spreadsheets in the workbook
   *
   * @return the number of sheets
   */
  int getNumberOfSheets();

  /**
   * Returns the index of the sheet by its name
   *
   * @param name the sheet name
   * @return index of the sheet (0 based)
   */
  int getSheetIndex(String name);

  /**
   * Get the sheet name
   *
   * @param sheet sheet number (0 based)
   * @return Sheet name
   */
  String getSheetName(int sheet);

  /**
   * @return the total number of defined names in this workbook
   */
  int getNumberOfNames();

  /**
   * Get all the range names in the workbook
   *
   * @return an array of range names
   */
  String[] getRangeNames();

  /**
   * Get the formula for a named range.
   *
   * @param name the name of the range.
   * @return the formula for the range or null if not found.
   */
  String getNameFormula(String name);

  /**
   * Get a sheet by its index
   *
   * @param sheetIndex the index of the sheet (0 based)
   * @return the sheet as an ExcelSheetReader object
   * @throws IllegalArgumentException if the sheet index is out of range.
   */
  ExcelSheetReader getSheetAt(int sheetIndex);

  /**
   * Close the underlying input resource (File or Stream), from which the Workbook was read.
   *
   * <p>Once this has been called, no further operations, updates or reads should be performed on
   * the Workbook.
   */
  void close() throws IOException;
}
