package org.enso.table.data.table;

import org.enso.table.data.column.Column;

import java.util.Arrays;

public class Table {
  public static class TableColumn {
    private final String name;
    private final Column column;

    public TableColumn(String name, Column column) {
      this.name = name;
      this.column = column;
    }

    public String getName() {
      return name;
    }

    public Column getColumn() {
      return column;
    }
  }

  private final TableColumn[] columns;

  public Table(TableColumn[] columns) {
    this.columns = columns;
  }

  public long nrows() {
    if (columns == null || columns.length == 0) {
      return 0;
    } else {
      return columns[0].column.size();
    }
  }

  public TableColumn[] getColumns() {
    return columns;
  }
}
