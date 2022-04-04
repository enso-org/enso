package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;

import java.util.List;

public class Last extends Aggregator {
  private final Storage storage;
  private final boolean ignoreNothing;

  public Last(String name, Column column, boolean ignoreNothing) {
    super(name, Storage.Type.OBJECT);
    this.storage = column.getStorage();
    this.ignoreNothing = ignoreNothing;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    for (int i = indexes.size() - 1; i >= 0; i--) {
      Object value = storage.getItemBoxed(indexes.get(i));
      if (!ignoreNothing || value != null) {
        return value;
      }
    }
    return null;
  }
}
