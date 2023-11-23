package org.enso.table.aggregations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.InvalidAggregation;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

import java.util.List;

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
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    ColumnAggregatedProblemAggregator innerAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
    Context context = Context.getCurrent();
    int count = 0;
    for (int row : indexes) {
      Object value = storage.getItemBoxed(row);
      if (value == null) {
        count += isEmpty ? 1 : 0;
      } else if (value instanceof String asString) {
        count += asString.isEmpty() == isEmpty ? 1 : 0;
      } else {
        innerAggregator.reportColumnAggregatedProblem(new InvalidAggregation(this.getName(), row, "Not a text value."));
        return null;
      }

      context.safepoint();
    }
    return count;
  }
}
