package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;

import java.util.List;

/***
 * Aggregate Column finding the first value in a group.
 */
public class First extends AggregateColumn {
  private final Storage storage;
  private final boolean ignoreNothing;

  public First(String name, Column column, boolean ignoreNothing) {
    super(name, Storage.Type.OBJECT);
    this.storage = column.getStorage();
    this.ignoreNothing = ignoreNothing;
  }

  @Override
  public Object aggregate(List<Integer> rows) {
    for (int row: rows) {
      Object value = storage.getItemBoxed(row);
      if (!ignoreNothing || value != null) {
        return value;
      }
    }
    return null;
  }
}
