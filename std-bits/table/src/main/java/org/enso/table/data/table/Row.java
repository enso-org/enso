package org.enso.table.data.table;

import java.util.Arrays;
import java.util.function.Function;
import org.enso.table.data.column.operation.JsonOperation;

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

  public String toJsonData(Function<Object, String> ensoJsonCallback) {
    StringBuilder sb = new StringBuilder();
    sb.append("{");
    for (int i = 0; i < column_count(); i++) {
      if (i > 0) {
        sb.append(",");
      }
      String name = table.getColumns()[i].getName();
      Object value = get_value(i, null);
      sb.append(JsonOperation.objectToJson(name, ensoJsonCallback))
          .append(":")
          .append(JsonOperation.objectToJson(value, ensoJsonCallback));
    }
    sb.append("}");
    return sb.toString();
  }
}
