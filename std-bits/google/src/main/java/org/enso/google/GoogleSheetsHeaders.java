package org.enso.google;

import com.google.api.services.sheets.v4.model.RowData;
import org.apache.poi.ss.util.CellReference;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.NameDeduplicator;

public class GoogleSheetsHeaders {
  private final NameDeduplicator deduplicator;
  private final String[] names;

  public GoogleSheetsHeaders(
      HeaderBehavior headerBehavior,
      RowData firstRow,
      RowData secondRow,
      ProblemAggregator problemAggregator) {
    deduplicator = NameDeduplicator.createDefault(problemAggregator);

    names =
        switch (headerBehavior) {
          case DEFAULT_COLUMN_NAMES -> null;
          case USE_FIRST_ROW_AS_HEADERS -> readFirstRowAsHeaders(firstRow, deduplicator);
          case INFER -> inferHeaders(firstRow, secondRow, deduplicator);
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

  private static String[] readFirstRowAsHeaders(RowData firstRow, NameDeduplicator deduplicator) {
    return firstRow.getValues().stream()
        .map(
            cell ->
                cell.getEffectiveValue() == null
                        || cell.getEffectiveValue().getStringValue() == null
                    ? ""
                    : cell.getEffectiveValue().getStringValue())
        .map(deduplicator::makeUnique)
        .toArray(String[]::new);
  }

  private static boolean cellIsString(com.google.api.services.sheets.v4.model.CellData cell) {
    return cell != null
        && ((cell.getEffectiveValue() != null
            && cell.getEffectiveValue().getStringValue() != null));
  }

  private static String[] inferHeaders(
      RowData firstRow, RowData secondRow, NameDeduplicator deduplicator) {
    // No data or 1 row of data => No Headers
    if (firstRow == null || firstRow.isEmpty() || secondRow == null) {
      return null;
    }

    boolean row1AllStrings =
        firstRow.getValues().stream().allMatch(GoogleSheetsHeaders::cellIsString);
    boolean row2AllStrings =
        secondRow.getValues().stream().allMatch(GoogleSheetsHeaders::cellIsString);

    if (!row1AllStrings) { // Row 1 has non string => no headers
      return null;
    } else if (row2AllStrings) { // Row 1 and Rows 2 all strings => no headers
      return null;
    } else { // Row 1 all strings and Rows 2 not all strings => headers
      return readFirstRowAsHeaders(firstRow, deduplicator);
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
