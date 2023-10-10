package org.enso.table.aggregations;

import java.util.List;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.ProblemAggregator;

/** Interface used to define aggregate columns. */
public abstract class Aggregator {
  private final String name;
  private final StorageType type;

  protected Aggregator(String name, StorageType type) {
    this.name = name;
    this.type = type;
  }

  /**
   * Return name of the new column
   *
   * @return Name of the new column.
   */
  public final String getName() {
    return name;
  }

  /**
   * Return type of the column
   *
   * @return The type of the new column.
   */
  public StorageType getType() {
    return type;
  }

  /**
   * Compute the value for a set of rows
   *
   * @param indexes - indexes to the rows in the source table to aggregate on
   * @return aggregated value
   */
  public abstract Object aggregate(List<Integer> indexes, ProblemAggregator problemAggregator);
}
