package org.enso.table.util;

import org.enso.table.data.column.storage.NullStorage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.problems.Problem;
import org.enso.table.problems.WithProblems;
import org.enso.table.util.problems.ColumnCountMismatch;
import org.enso.table.util.problems.ColumnNameMismatch;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


public class ColumnMapper {
  public static WithProblems<Table> MapColumnsByName(Table table, String[] columnNames) {
    Column[] columns = new Column[columnNames.length];
    Set<String> extras = new HashSet<>(Arrays.asList(columnNames));
    Set<String> missing = new HashSet<>();

    for (int i = 0; i < columnNames.length; i++) {
      String name = columnNames[i];
      Column column = table.getColumnByName(name);
      if (column == null) {
        missing.add(name);
        NullStorage storage = new NullStorage(columns[0].getStorage().size());
        columns[i] = new Column(java.util.UUID.randomUUID().toString(), storage);
      } else {
        extras.remove(name);
        columns[i] = column;
      }
    }

    Table newTable = new Table(columns);
    if (missing.isEmpty() && extras.isEmpty()) {
      return new WithProblems<>(newTable, Collections.emptyList());
    } else {
      Problem problem = new ColumnNameMismatch(missing.toArray(String[]::new), extras.toArray(String[]::new));
      return new WithProblems<>(newTable, List.of(problem));
    }
  }

  public static WithProblems<Table> MapColumnsByIndex(Table table, int columnCount) {
    Column[] columns = table.getColumns();
    if (columns.length == columnCount) {
      return new WithProblems<>(table, Collections.emptyList());
    }

    Column[] newColumns = new Column[columnCount];
    System.arraycopy(columns, 0, newColumns, 0, Math.min(columnCount, columns.length));
    if (columns.length < columnCount) {
      NullStorage storage = new NullStorage(columns[0].getStorage().size());
      Arrays.fill(newColumns, columns.length, columnCount, new Column(java.util.UUID.randomUUID().toString(), storage));
    }

    Problem problem = new ColumnCountMismatch(columnCount, columns.length);
    return new WithProblems<>(new Table(newColumns), List.of(problem));
  }
}
