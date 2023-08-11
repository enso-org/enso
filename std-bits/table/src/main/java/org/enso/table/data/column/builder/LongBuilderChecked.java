package org.enso.table.data.column.builder;

import java.util.BitSet;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.cast.CastProblemBuilder;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.AggregatedProblems;

/** A LongBuilder that ensures values it is given fit the target type. */
public class LongBuilderChecked extends LongBuilder {
  private final IntegerType type;
  private final CastProblemBuilder castProblemBuilder;

  protected LongBuilderChecked(BitSet isMissing, long[] data, int currentSize, IntegerType type) {
    super(isMissing, data, currentSize);
    this.type = type;

    // Currently we have no correlation with column name, and it may not be necessary for now.
    String relatedColumnName = null;
    this.castProblemBuilder = new CastProblemBuilder(relatedColumnName, type);
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      isMissing.set(currentSize++);
    } else {
      long x = NumericConverter.coerceToLong(o);
      appendLongNoGrow(x);
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
      castProblemBuilder.reportNumberOutOfRange(x);
    }
  }

  @Override
  public AggregatedProblems getProblems() {
    return castProblemBuilder.getAggregatedProblems();
  }
}
