package org.enso.table.aggregations;

import java.util.List;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.problems.ProblemAggregator;

/** Interface used to define aggregate columns. */
public abstract class Aggregator {
  private final String name;

  protected Aggregator(String name) {
    this.name = name;
  }

  /**
   * Return name of the new column
   *
   * @return Name of the new column.
   */
  public final String getName() {
    return name;
  }

  /** Creates a builder that can hold results of this aggregator. */
  public abstract Builder makeBuilder(int size, ProblemAggregator problemAggregator);

  /**
   * Compute the value for a set of rows
   *
   * @param indexes - indexes to the rows in the source table to aggregate on
   * @return aggregated value
   */
  public abstract Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator);
}
