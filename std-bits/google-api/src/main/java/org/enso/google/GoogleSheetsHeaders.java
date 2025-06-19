package org.enso.google;

import java.util.List;

import org.apache.poi.ss.util.CellReference;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.NameDeduplicator;

public class GoogleSheetsHeaders {
  private final NameDeduplicator deduplicator;
  private final String[] names;

  public GoogleSheetsHeaders(
      HeaderBehavior headers,
      List<List<Object>> firstTwoRows,
      ProblemAggregator problemAggregator) {
    deduplicator = NameDeduplicator.createDefault(problemAggregator);

    names =
        switch (headers) {
          case DEFAULT_COLUMN_NAMES -> null;
          case USE_FIRST_ROW_AS_HEADERS -> readRowAsHeaders(firstTwoRows, deduplicator);
          case INFER -> readRowAsHeaders(firstTwoRows, deduplicator);
        };
  }

  public String get(int column) {
    if (names == null) {
      return CellReference.convertNumToColString(column);
    }

    String name = column < names.length ? names[column] : "";
    if (name == null || name.isEmpty()) {
      name = deduplicator.makeUnique(name);
    }
    return name;
  }

  public int getRowsUsed() {
    return this.names == null ? 0 : 1;
  }

  private static String[] readRowAsHeaders(
      List<List<Object>> firstTwoRows, NameDeduplicator deduplicator) {
    if (firstTwoRows == null) {
      return null;
    }

    if (firstTwoRows.isEmpty()) {
      return null;
    }

    List<Object> firstRow = firstTwoRows.get(0);
    String[] headers = new String[firstRow.size()];
    for (int i = 0; i < firstRow.size(); i++) {
      Object cell = firstRow.get(i);
      String name = cell == null ? "" : cell.toString().trim();
      headers[i] = deduplicator.makeUnique(name);
    }
    return headers;
  }

  private static String[] inferHeaders(
      List<List<Object>> firstTwoRows, NameDeduplicator deduplicator) {
    // if (row == null || nextRow == null) {
    //   return null;
    // }

    // String[] rowNames = row.getCellsAsText(startCol, endCol);
    // if (rowNames == null) {
    //   return null;
    // }

    // if (nextRow.getCellsAsText(startCol, endCol) != null) {
    //   return null;
    // }

    // return readRowAsHeaders(row, startCol, endCol, deduplicator);
    return null;
  }

  /** Specifies how to set the headers for the returned table. */
  public enum HeaderBehavior {
    /** Tries to infer if the headers are present in the file. */
    INFER,

    /** Uses the first row in the file as headers. Duplicate names will be appended suffixes. */
    USE_FIRST_ROW_AS_HEADERS,

    /** Uses the default Column Names (e.g. A, B, C). */
    DEFAULT_COLUMN_NAMES
  }
}
