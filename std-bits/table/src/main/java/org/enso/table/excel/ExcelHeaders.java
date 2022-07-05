package org.enso.table.excel;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.util.CellReference;
import org.enso.table.problems.Problem;
import org.enso.table.util.NameDeduplicator;

import java.util.List;

public class ExcelHeaders {
  private final NameDeduplicator deduplicator;
  private final int startCol;
  private final String[] names;

  public ExcelHeaders(HeaderBehavior headers, ExcelRow startRow, ExcelRow nextRow, int startCol, int endCol) {
    deduplicator = new NameDeduplicator();

    this.startCol = startCol;
    names = switch (headers) {
      case EXCEL_COLUMN_NAMES -> null;
      case USE_FIRST_ROW_AS_HEADERS -> readRowAsHeaders(startRow, startCol, endCol, deduplicator);
      case INFER -> inferHeaders(startRow, nextRow, startCol, endCol, deduplicator);
    };
  }

  public String get(int column) {
    if (names == null) {
      return CellReference.convertNumToColString(column - 1);
    }

    int idx = column - startCol;
    String name = idx < names.length ? names[idx] : "";
    if (name == null || name.isEmpty()) {
      name = deduplicator.makeUnique(name);
    }
    return name;
  }

  public int getRowsUsed() {
    return this.names == null ? 0 : 1;
  }

  public List<Problem> getProblems() {
    return deduplicator.getProblems();
  }

  private static String[] readRowAsHeaders(ExcelRow row, int startCol, int endCol, NameDeduplicator deduplicator) {
    if (row == null) {
      return null;
    }

    int currentEndCol = endCol == -1 ? row.getLastColumn() : endCol;
    DataFormatter formatter = new DataFormatter();

    String[] output = new String[currentEndCol - startCol + 1];
    for (int col = startCol; col <= currentEndCol; col++) {
      Cell cell = row.get(col);

      String name = cell == null ? "" : formatter.formatCellValue(cell);
      if (!name.isEmpty()) {
        name = deduplicator.makeUnique(name);
      }

      output[col - startCol] = name;
    }

    return output;
  }

  private static String[] inferHeaders(ExcelRow row, ExcelRow nextRow, int startCol, int endCol, NameDeduplicator deduplicator) {
    if (row == null || nextRow == null) {
      return null;
    }

    String[] rowNames = row.getCellsAsText(startCol, endCol);
    if (rowNames == null) {
      return null;
    }

    if (nextRow.getCellsAsText(startCol, endCol) != null) {
      return null;
    }

    return deduplicator.makeUnique(rowNames);
  }

  /** Specifies how to set the headers for the returned table. */
  public enum HeaderBehavior {
    /** Tries to infer if the headers are present in the file. */
    INFER,

    /** Uses the first row in the file as headers. Duplicate names will be appended suffixes. */
    USE_FIRST_ROW_AS_HEADERS,

    /** Uses the default Excel Column Names (e.g. A, B, C). */
    EXCEL_COLUMN_NAMES
  }
}
