package org.enso.table.data.column.builder;

import org.enso.table.data.column.operation.cast.CastProblemAggregator;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.ProblemAggregator;

/** A LongBuilder that ensures values it is given fit the target type. */
final class BoundCheckedIntegerBuilder extends LongBuilder {
  private final IntegerType type;
  private final CastProblemAggregator castProblemAggregator;

  protected BoundCheckedIntegerBuilder(
      int initialSize, IntegerType type, ProblemAggregator problemAggregator) {
    super(initialSize, problemAggregator);
    this.type = type;

    // Currently we have no correlation with column name, and it may not be necessary for now.
    // TODO ideally we want to pass the column through a problem aggregator context
    String relatedColumnName = null;
    this.castProblemAggregator =
        new CastProblemAggregator(problemAggregator, relatedColumnName, type);
  }

  @Override
  public BoundCheckedIntegerBuilder appendLong(long value) {
    if (!type.fits(value)) {
      castProblemAggregator.reportNumberOutOfRange(value);
      appendNulls(1);
      return this;
    }
    super.appendLong(value);
    return this;
  }

  @Override
  public IntegerType getType() {
    return type;
  }
}
