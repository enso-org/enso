package org.enso.table.util;

import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.error.ColumnCountMismatchException;
import org.enso.table.error.ColumnNameMismatchException;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class ColumnMapper {
  /**
   * Match the table's columns against the list of column names and return a reordered table.
   *
   * @param table the table of data to append.
   * @param columnNames set of existing column names.
   * @return a new table with columns reordered to match to the given column names.
   * @throws ColumnNameMismatchException if the names in the new table do not match the existing
   *     set.
   */
  public static Table mapColumnsByName(Table table, String[] columnNames)
      throws ColumnNameMismatchException {
    Column[] columns = new Column[columnNames.length];
    Set<String> extras =
        Arrays.stream(table.getColumns()).map(Column::getName).collect(Collectors.toSet());
    Set<String> missing = new HashSet<>();

    for (int i = 0; i < columnNames.length; i++) {
      String name = columnNames[i];
      Column column = table.getColumnByName(name);
      if (column == null) {
        missing.add(name);
      } else {
        extras.remove(name);
        columns[i] = column;
      }
    }

    if (missing.isEmpty() && extras.isEmpty()) {
      return new Table(columns);
    } else {
      throw new ColumnNameMismatchException(
          missing.toArray(String[]::new), extras.toArray(String[]::new));
    }
  }

  /**
   * Check the column count of the table, if matches return otherwise throw an exception.
   *
   * @param table the table of data to append.
   * @param columnCount exisitng column count.
   * @return the input table if column count matches.
   * @throws ColumnCountMismatchException if the column counts do not match.
   */
  public static Table mapColumnsByPosition(Table table, int columnCount)
      throws ColumnCountMismatchException {
    int column_length = table.getColumns().length;
    if (column_length == columnCount) {
      return table;
    }

    throw new ColumnCountMismatchException(columnCount, column_length);
  }
}
