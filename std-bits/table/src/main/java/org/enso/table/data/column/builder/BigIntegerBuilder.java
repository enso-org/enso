package org.enso.table.data.column.builder;

import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

public class BigIntegerBuilder extends TypedBuilder<BigInteger> {
  // The problem aggregator is only used so that when we are retyping, we can pass it on.
  private final ProblemAggregator problemAggregator;

  BigIntegerBuilder(int size, ProblemAggregator problemAggregator) {
    super(BigIntegerType.INSTANCE, new BigInteger[size]);
    this.problemAggregator = problemAggregator;
  }

  @Override
  public boolean canRetypeTo(StorageType type) {
    return type instanceof FloatType
        || type instanceof BigDecimalType;
  }

  @Override
  public Builder retypeTo(StorageType type) {
    switch (type) {
      case FloatType _ -> {
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
      case BigDecimalType _ -> {
        var res = Builder.getForType(type, data.length, problemAggregator);
        for (int i = 0; i < currentSize; i++) {
          if (data[i] == null) {
            res.appendNulls(1);
          } else {
            res.appendNoGrow(data[i]);
          }
        }
        return res;
      }
      default -> throw new UnsupportedOperationException();
    }
  }

  @Override
  protected Storage<BigInteger> doSeal() {
    return new BigIntegerStorage(data, currentSize);
  }

  @Override
  public boolean accepts(Object o) {
    return NumericConverter.isCoercibleToBigInteger(o);
  }

  @Override
  public void appendNoGrow(Object o) {
    if (o == null) {
      data[currentSize++] = null;
    } else if (o instanceof BigInteger value) {
      data[currentSize++] = value;
    } else {
      try {
        data[currentSize++] = NumericConverter.coerceToBigInteger(o);
      } catch (UnsupportedOperationException e) {
        throw new ValueTypeMismatchException(BigIntegerType.INSTANCE, o);
      }
    }
  }

  public static Builder retypeFromLongBuilder(LongBuilder longBuilder) {
    var res = new BigIntegerBuilder(longBuilder.data.length, longBuilder.problemAggregator);
    int n = longBuilder.currentSize;
    Context context = Context.getCurrent();
    for (int i = 0; i < n; i++) {
      res.appendNoGrow(BigInteger.valueOf(longBuilder.data[i]));
      context.safepoint();
    }
    return res;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    if (storage.getType() instanceof IntegerType) {
      if (storage instanceof AbstractLongStorage longStorage) {
        int n = longStorage.size();
        for (int i = 0; i < n; i++) {
          if (storage.isNothing(i)) {
            data[currentSize++] = null;
          } else {
            long item = longStorage.getItem(i);
            data[currentSize++] = BigInteger.valueOf(item);
          }
        }
      } else {
        throw new IllegalStateException(
            "Unexpected storage implementation for type INTEGER: "
                + storage
                + ". This is a bug in the Table library.");
      }
    } else {
      super.appendBulkStorage(storage);
    }
  }
}
