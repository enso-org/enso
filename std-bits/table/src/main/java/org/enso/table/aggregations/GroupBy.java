package org.enso.table.aggregations;

import java.util.List;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

/** Aggregate Column getting the grouping key. */
public class GroupBy extends Aggregator {
  private final Storage<?> storage;

  public GroupBy(String name, Column column) {
    super(name, column.getStorage().getType());
    storage = column.getStorage();
  }

  @Override
  public Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator) {
    return indexes.isEmpty() ? null : storage.getItemBoxed(indexes.get(0));
  }
}
