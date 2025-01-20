package org.enso.table.data.column.builder;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.cast.CastProblemAggregator;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;

/** A LongBuilder that ensures values it is given fit the target type. */
public class BoundCheckedIntegerBuilder extends LongBuilder {
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
  public void appendLong(long value) {
    if (!type.fits(value)) {
      castProblemAggregator.reportNumberOutOfRange(value);
      appendNulls(1);
      return;
    }
    super.appendLong(value);
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      isNothing.set(currentSize++);
    } else {
      Long x = NumericConverter.tryConvertingToLong(o);
      if (x != null) {
        if (type.fits(x)) {
          this.data[currentSize++] = x;
        } else {
          isNothing.set(currentSize++);
          castProblemAggregator.reportNumberOutOfRange(x);
        }
      } else {
        throw new ValueTypeMismatchException(type, o);
      }
    }
  }

  @Override
  public IntegerType getType() {
    return type;
  }
}
