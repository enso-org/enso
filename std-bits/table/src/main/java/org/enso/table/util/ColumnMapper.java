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
  public static Table MapColumnsByName(Table table, String[] columnNames)
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

    Table newTable = new Table(columns);
    if (missing.isEmpty() && extras.isEmpty()) {
      return newTable;
    } else {
      throw new ColumnNameMismatchException(
          missing.toArray(String[]::new), extras.toArray(String[]::new));
    }
  }

  public static Table MapColumnsByIndex(Table table, int columnCount)
      throws ColumnCountMismatchException {
    Column[] columns = table.getColumns();
    if (columns.length == columnCount) {
      return table;
    }

    throw new ColumnCountMismatchException(columnCount, columns.length);
  }
}
