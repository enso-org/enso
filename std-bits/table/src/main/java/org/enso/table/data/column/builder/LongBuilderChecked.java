package org.enso.table.data.column.builder;

import java.util.BitSet;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.cast.CastProblemAggregator;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;

/** A LongBuilder that ensures values it is given fit the target type. */
public class LongBuilderChecked extends LongBuilder {
  private final IntegerType type;
  private final CastProblemAggregator castProblemAggregator;

  protected LongBuilderChecked(
      BitSet isMissing,
      long[] data,
      int currentSize,
      IntegerType type,
      ProblemAggregator problemAggregator) {
    super(isMissing, data, currentSize, problemAggregator);
    this.type = type;

    // Currently we have no correlation with column name, and it may not be necessary for now.
    // TODO ideally we want to pass the column through a problem aggregator context
    String relatedColumnName = null;
    this.castProblemAggregator =
        new CastProblemAggregator(problemAggregator, relatedColumnName, type);
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      isMissing.set(currentSize++);
    } else {
      Long x = NumericConverter.tryConvertingToLong(o);
      if (x != null) {
        appendLongNoGrow(x);
      } else {
        throw new ValueTypeMismatchException(type, o);
      }
    }
  }

  @Override
  public IntegerType getType() {
    return type;
  }

  @Override
  public void appendLongNoGrow(long x) {
    if (type.fits(x)) {
      data[currentSize++] = x;
    } else {
      isMissing.set(currentSize++);
      castProblemAggregator.reportNumberOutOfRange(x);
    }
  }
}
