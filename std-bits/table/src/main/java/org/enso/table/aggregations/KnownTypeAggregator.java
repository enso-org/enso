package org.enso.table.aggregations;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.ProblemAggregator;

/**
 * A common subclass for aggregators that know their type on construction and use a standard
 * builder.
 */
public abstract class KnownTypeAggregator extends Aggregator {
  private final StorageType type;

  protected KnownTypeAggregator(String name, StorageType type) {
    super(name);
    this.type = type;
  }

  @Override
  public Builder makeBuilder(int size, ProblemAggregator problemAggregator) {
    return Builder.getForType(type, size, problemAggregator);
  }

  /**
   * Return type of the column
   *
   * @return The type of the new column.
   */
  public StorageType getType() {
    return type;
  }
}
