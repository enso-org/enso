package org.enso.table.data.column.builder;

import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

final class BigIntegerBuilder extends TypedBuilder<BigInteger> {
  // The problem aggregator is only used so that when we are retyping, we can pass it on.
  private final ProblemAggregator problemAggregator;

  BigIntegerBuilder(int size, ProblemAggregator problemAggregator) {
    super(BigIntegerType.INSTANCE, new BigInteger[size]);
    this.problemAggregator = problemAggregator;
  }

  @Override
  public boolean canRetypeTo(StorageType<?> type) {
    return type instanceof FloatType || type instanceof BigDecimalType;
  }

  @Override
  public Builder retypeTo(StorageType<?> type) {
    switch (type) {
      case FloatType floatType -> {
        // Needs to be an InferredDoubleBuilder so we can keep the raw data.
        var res = new InferredDoubleBuilder(currentSize, problemAggregator);
        for (int i = 0; i < currentSize; i++) {
          if (data[i] == null) {
            res.appendNulls(1);
          } else {
            res.append(data[i]);
          }
        }
        return res;
      }
      case BigDecimalType bigDecimalType -> {
        var res = Builder.getForBigDecimal(data.length);
        for (int i = 0; i < currentSize; i++) {
          if (data[i] == null) {
            res.appendNulls(1);
          } else {
            res.append(data[i]);
          }
        }
        return res;
      }
      default -> throw new UnsupportedOperationException();
    }
  }

  @Override
  protected ColumnStorage<BigInteger> doSeal() {
    return new TypedStorage<>(BigIntegerType.INSTANCE, data);
  }

  @Override
  public boolean accepts(Object o) {
    return NumericConverter.isCoercibleToBigInteger(o);
  }

  @Override
  public BigIntegerBuilder append(Object o) {
    ensureSpaceToAppend();

    if (o == null) {
      appendNulls(1);
    } else {
      try {
        data[currentSize++] = NumericConverter.coerceToBigInteger(o);
      } catch (UnsupportedOperationException e) {
        throw new ValueTypeMismatchException(BigIntegerType.INSTANCE, o);
      }
    }

    return this;
  }

  static Builder retypeFromLongBuilder(
      BuilderForLong longBuilder, ProblemAggregator problemAggregator) {
    var res = Builder.getForBigInteger(longBuilder.getCurrentCapacity(), problemAggregator);
    long n = longBuilder.getCurrentSize();
    Context context = Context.getCurrent();
    for (long i = 0; i < n; i++) {
      res.append(longBuilder.isNothing(i) ? null : BigInteger.valueOf(longBuilder.getLong(i)));
      context.safepoint();
    }
    return res;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    if (storage instanceof ColumnLongStorage longStorage) {
      long n = longStorage.getSize();
      for (long i = 0; i < n; i++) {
        if (storage.isNothing(i)) {
          appendNulls(1);
        } else {
          long item = longStorage.getItemAsLong(i);
          append(BigInteger.valueOf(item));
        }
      }
    } else {
      super.appendBulkStorage(storage);
    }
  }
}
