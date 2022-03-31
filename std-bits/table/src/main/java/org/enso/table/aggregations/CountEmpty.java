package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;

import java.util.List;

/***
 * Aggregate Column counting the number of (not-)null entries in a group.
 */
public class CountEmpty extends AggregateColumn {
  private final Storage storage;
  private final boolean isEmpty;

  public CountEmpty(String name, Column column, boolean isEmpty) {
    super(name, Storage.Type.LONG);
    this.storage = column.getStorage();
    this.isEmpty = isEmpty;
  }

  @Override
  public Object aggregate(List<Integer> rows) {
    int count = 0;
    for (int row : rows) {
      Object value = storage.getItemBoxed(row);
      if (value != null && !(value instanceof String)) {
        return new InvalidAggregation(this.getName(), row, "Non-Text value - cannot Count " + (isEmpty ? "Empty" : "Non-Empty"));
      }

      count += ((value == null || ((String) value).length() == 0) == isEmpty ? 1 : 0);
    }
    return count;
  }
}
