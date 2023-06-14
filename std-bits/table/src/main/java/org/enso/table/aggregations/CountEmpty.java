package org.enso.table.aggregations;

import java.util.List;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;

/**
 * Aggregate Column counting the number of (non-)empty entries in a group. If `isEmpty` is true,
 * counts null or empty entries. If `isEmpty` is false, counts non-empty entries.
 */
public class CountEmpty extends Aggregator {
  private final Storage<?> storage;
  private final boolean isEmpty;

  /**
   * Constructs a CountNothing Aggregator
   *
   * @param name output column name
   * @param column input column
   * @param isEmpty true to count nulls or empty, false to count non-empty
   */
  public CountEmpty(String name, Column column, boolean isEmpty) {
    super(name, IntegerType.INT_64);
    this.storage = column.getStorage();
    this.isEmpty = isEmpty;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    int count = 0;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value != null && !(value instanceof String)) {
        this.addProblem(new InvalidAggregation(this.getName(), row, "Not a text value."));
        return null;
      }

      count += ((value == null || ((String) value).length() == 0) == isEmpty ? 1 : 0);
    }
    return count;
  }
}
