package org.enso.table.data.table;

import java.util.Arrays;
import java.util.function.Function;
import java.util.function.Supplier;

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

  public long index() {
    return rowIndex;
  }

  public String[] column_names() {
    return Arrays.stream(table.getColumns()).map(Column::getName).toArray(String[]::new);
  }

  public long column_count() {
    return table.getColumns().length;
  }

  public Object get_value(int index, Function<Object, Object> ifMissing) {
    if (index < 0 || index >= column_count()) {
      return ifMissing.apply(index);
    }
    return table.getColumns()[index].getItem(rowIndex);
  }

  public Object get_value(String name, Function<Object, Object> ifMissing) {
    var column = table.getColumnByName(name);
    return column == null ? ifMissing.apply(name) : column.getItem(rowIndex);
  }
}
