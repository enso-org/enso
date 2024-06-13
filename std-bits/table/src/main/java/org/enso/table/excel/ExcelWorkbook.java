package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Workbook;

/** Wrapper class to handle Excel workbooks.
 * Wraps either a POI Workbook
 * */
public interface ExcelWorkbook {
  int getNumberOfSheets();

  String getSheetName(int index);

  int getSheetIndex(String name);

  String[] getAllRangeNames();

  Name getNamedRange(String Name);

  static ExcelWorkbook fromWorkbook(Workbook workbook) {
    return new ExcelWorkbookImpl(workbook);
  }

  record ExcelWorkbookImpl(Workbook workbook) implements ExcelWorkbook {
    @Override
    public int getNumberOfSheets() {
      return workbook.getNumberOfSheets();
    }

    @Override
    public String getSheetName(int index) {
      return workbook.getSheetName(index);
    }

    @Override
    public int getSheetIndex(String name) { return workbook.getSheetIndex(name); }

    @Override
    public String[] getAllRangeNames() {
      return workbook.getAllNames().stream().map(Name::getNameName).toArray(String[]::new);
    }

    @Override
    public Name getNamedRange(String name) { return workbook.getName(name); }
  }
}
