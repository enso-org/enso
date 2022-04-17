package org.enso.table.data.table.aggregate;

import org.enso.table.data.column.operation.aggregate.Aggregator;
import org.enso.table.data.index.Index;
import org.enso.table.data.table.Column;

import java.util.List;
import java.util.function.Function;
import java.util.stream.IntStream;

/** A column wrapper used for aggregation operations. */
public class AggregateColumn {
  private final Index uniqueIndex;
  private final Column column;

  /**
   * Creates a new column
   *
   * @param uniqueIndex the unique index obtained from the column's index
   * @param column the wrapped column
   */
  public AggregateColumn(Index uniqueIndex, Column column) {
    this.uniqueIndex = uniqueIndex;
    this.column = column;
  }

  /**
   * Aggregates the groups using a given aggregation operation.
   *
   * @param aggName name of a vectorized operation that can be used if possible. If null is passed,
   *     this parameter is unused.
   * @param outSuffix a string appended to the name of the resulting column.
   * @param aggregatorFunction the function to use if a vectorized operation is not available.
   * @param skipNa whether missing values should be passed to the {@code fallback} function.
   * @return a column indexed by the unique index of this aggregate, storing results of applying the
   *     specified operation.
   */
  public Column aggregate(
      String aggName,
      String outSuffix,
      Function<List<Object>, Object> aggregatorFunction,
      boolean skipNa) {
    Aggregator aggregator =
        column.getStorage().getAggregator(aggName, aggregatorFunction, skipNa, uniqueIndex.size());

    for (int i = 0; i < uniqueIndex.size(); i++) {
      IntStream ixes =
          column.getIndex().loc(uniqueIndex.iloc(i)).stream().mapToInt(Integer::intValue);
      aggregator.nextGroup(ixes);
    }
    return new Column(column.getName() + outSuffix, uniqueIndex, aggregator.seal());
  }

  /** @return the underlying (ungrouped) column. */
  public Column getColumn() {
    return column;
  }
}
