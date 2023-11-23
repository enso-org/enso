package org.enso.table.aggregations;

import java.util.List;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/**
 * Aggregate Column counting the number of (not-)null entries in a group. If `isNothing` is true,
 * counts null entries. If `isNothing` is false, counts non-null entries.
 */
public class CountNothing extends Aggregator {
  private final Storage<?> storage;
  private final boolean isNothing;

  /**
   * Constructs a CountNothing Aggregator
   *
   * @param name output column name
   * @param column input column
   * @param isNothing true to count nulls, false to count non-nulls
   */
  public CountNothing(String name, Column column, boolean isNothing) {
    super(name, IntegerType.INT_64);
    this.storage = column.getStorage();
    this.isNothing = isNothing;
  }

  @Override
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    long count = 0;
    for (int row : indexes) {
      count += ((storage.getItemBoxed(row) == null) == isNothing ? 1L : 0L);
      context.safepoint();
    }
    return count;
  }
}
