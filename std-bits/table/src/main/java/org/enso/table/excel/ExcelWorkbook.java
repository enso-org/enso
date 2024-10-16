package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Name;

import java.io.IOException;

/** Represents an Excel workbook. Wraps the underlying Apache POI Workbook object. */
public interface ExcelWorkbook {
  /**
   * Get the number of spreadsheets in the workbook
   *
   * @return the number of sheets
   */
  int getNumberOfSheets();

  /**
   * Returns the index of the sheet by his name
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
   * @return the sheet as an ExcelSheet object
   * @throws IllegalArgumentException if the sheet index is out of range.
   */
  ExcelSheet getSheetAt(int sheetIndex);

  /**
   * Close the underlying input resource (File or Stream),
   *  from which the Workbook was read.
   *
   * <p>Once this has been called, no further
   *  operations, updates or reads should be performed on the
   *  Workbook.
   */
  void close() throws IOException;

  /**
   * Create an ExcelWorkbook object from an Apache POI Workbook object
   *
   * @param workbook the Apache POI Workbook object
   * @return the ExcelWorkbook object
   */
  static ExcelWorkbook forWorkbook(org.apache.poi.ss.usermodel.Workbook workbook) {
    return new ExcelWorkbookForWorkbook(workbook);
  }

  // ** Wrap a Workbook object in the interface. */
  record ExcelWorkbookForWorkbook(org.apache.poi.ss.usermodel.Workbook workbook)
      implements ExcelWorkbook {
    @Override
    public int getNumberOfSheets() {
      return workbook.getNumberOfSheets();
    }

    @Override
    public int getSheetIndex(String name) {
      return workbook.getSheetIndex(name);
    }

    @Override
    public String getSheetName(int sheet) {
      return workbook.getSheetName(sheet);
    }

    @Override
    public int getNumberOfNames() {
      return workbook.getNumberOfNames();
    }

    @Override
    public String[] getRangeNames() {
      var names = workbook.getAllNames();
      return names.stream().map(Name::getNameName).toArray(String[]::new);
    }

    @Override
    public String getNameFormula(String name) {
      var namedRange = workbook.getName(name);
      return namedRange == null ? null : namedRange.getRefersToFormula();
    }

    @Override
    public ExcelSheet getSheetAt(int sheetIndex) {
      return ExcelSheet.fromWorkbook(workbook, sheetIndex);
    }

    @Override
    public void close() throws IOException {
      workbook.close();
    }
  }
}
