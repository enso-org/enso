package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;

import java.util.List;
import java.util.OptionalInt;
import java.util.stream.IntStream;

/***
 * Aggregate Column getting the grouping key.
 */
public class GroupBy extends AggregateColumn {
  private final Storage storage;

  public GroupBy(String name, Column column) {
    super(name, Storage.Type.OBJECT);
    storage = column.getStorage();
  }

  @Override
  public Object aggregate(List<Integer> rows) {
    return rows.isEmpty() ? null : storage.getItemBoxed(rows.get(0));
  }
}
