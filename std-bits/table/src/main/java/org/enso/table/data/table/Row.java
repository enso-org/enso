package org.enso.table.data.table;

import java.util.Arrays;
import java.util.function.Function;

public class Row {
  private final Table table;
  private long rowIndex;

  public Row(Table table, long rowIndex) {
    this.table = table;
    this.rowIndex = rowIndex;
  }

  public void setRowIndex(long rowIndex) {
    this.rowIndex = rowIndex;
  }

  public Table table() {
    return table;
  }

  public long index() {
    return rowIndex;
  }

  public String[] column_names() {
    return Arrays.stream(table.getColumns()).map(Column::getName).toArray(String[]::new);
  }

  public int column_count() {
    return table.getColumns().length;
  }

  public String get_name(int index) {
    return table.getColumns()[index].getName();
  }

  public Object get_value(int index, Function<Object, Object> ifMissing) {
    var count = column_count();
    if (index < -count || index >= count) {
      return ifMissing.apply(index);
    }
    if (index < 0) {
      index += count;
    }
    return table.getColumns()[index].getItem(rowIndex);
  }

  public Object get_value(String name, Function<Object, Object> ifMissing) {
    var column = table.getColumnByName(name);
    return column == null ? ifMissing.apply(name) : column.getItem(rowIndex);
  }
}
