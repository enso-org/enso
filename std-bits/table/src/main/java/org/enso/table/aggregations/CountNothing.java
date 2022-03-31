package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;

import java.util.List;

/***
 * Aggregate Column counting the number of (not-)null entries in a group.
 */
public class CountNothing extends AggregateColumn {
  private final Storage storage;
  private final boolean isNothing;

  public CountNothing(String name, Column column, boolean isNothing) {
    super(name, Storage.Type.LONG);
    this.storage = column.getStorage();
    this.isNothing = isNothing;
  }

  @Override
  public Object aggregate(List<Integer> rows) {
    long count = 0;
    for (int row: rows) {
      count += ((storage.getItemBoxed(row) == null) == isNothing ? 1 : 0);
    }
    return count;
  }
}

