package org.enso.table.data.column.builder;

import java.util.BitSet;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;

/**
 * A LongBuilder that does not need to check value ranges, because it stores 64-bit integers and no
 * larger integers will come to appendLong anyway.
 */
public class LongBuilderUnchecked extends LongBuilder {
  protected LongBuilderUnchecked(
      BitSet isMissing, long[] data, int currentSize, ProblemAggregator problemAggregator) {
    super(isMissing, data, currentSize, problemAggregator);
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
        throw new ValueTypeMismatchException(getType(), o);
      }
    }
  }

  @Override
  public void appendLongNoGrow(long data) {
    appendRawNoGrow(data);
  }

  @Override
  public IntegerType getType() {
    return IntegerType.INT_64;
  }
}
