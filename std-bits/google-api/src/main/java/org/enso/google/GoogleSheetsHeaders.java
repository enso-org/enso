package org.enso.google;

import java.util.List;
import org.apache.poi.ss.util.CellReference;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.NameDeduplicator;

public class GoogleSheetsHeaders {
  private final NameDeduplicator deduplicator;
  private final String[] names;

  public GoogleSheetsHeaders(
      HeaderBehavior headerBehavior,
      List<List<Object>> rawData,
      ProblemAggregator problemAggregator) {
    deduplicator = NameDeduplicator.createDefault(problemAggregator);

    names =
        switch (headerBehavior) {
          case DEFAULT_COLUMN_NAMES -> null;
          case USE_FIRST_ROW_AS_HEADERS -> readFirstRowAsHeaders(rawData, deduplicator);
          case INFER -> inferHeaders(rawData, deduplicator);
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

  private static String[] readFirstRowAsHeaders(
      List<List<Object>> rawData, NameDeduplicator deduplicator) {
    return rawData.stream()
        .map(
            column -> {
              Object cell = column.stream().findFirst().orElse(null);
              String name = cell == null ? "" : cell.toString();
              return deduplicator.makeUnique(name);
            })
        .toArray(String[]::new);
  }

  private static String[] inferHeaders(List<List<Object>> rawData, NameDeduplicator deduplicator) {
    // No data or 1 row of data => No Headers
    if (rawData == null || rawData.isEmpty() || rawData.get(0).size() == 1) {
      return null;
    }

    boolean row1AllStrings = rawData.stream().allMatch(col -> col.get(0) instanceof String);
    boolean row2AllStrings = rawData.stream().allMatch(col -> col.get(1) instanceof String);

    if (!row1AllStrings) { // Row 1 has non string => no headers
      return null;
    } else if (row2AllStrings) { // Row 1 and Rows 2 all strings => no headers
      return null;
    } else { // Row 1 all strings and Rows 2 not all strings => headers
      return readFirstRowAsHeaders(rawData, deduplicator);
    }
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
