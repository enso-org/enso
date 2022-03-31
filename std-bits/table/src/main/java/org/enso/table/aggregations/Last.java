package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;

import java.util.List;

public class Last extends AggregateColumn {
  private final Storage storage;
  private final boolean ignoreNothing;

  public Last(String name, Column column, boolean ignoreNothing) {
    super(name, Storage.Type.OBJECT);
    this.storage = column.getStorage();
    this.ignoreNothing = ignoreNothing;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    Object current = null;
    for (int row: indexes) {
      Object value = storage.getItemBoxed(row);
      if (!ignoreNothing || value != null) {
        current = value;
      }
    }
    return current;
  }
}