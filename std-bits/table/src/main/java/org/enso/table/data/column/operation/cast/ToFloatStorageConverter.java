package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.ObjLongConsumer;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForDouble;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.graalvm.polyglot.Context;

public class ToFloatStorageConverter implements StorageConverter<Double> {
  public ToFloatStorageConverter(FloatType targetType) {
    if (targetType.bits() != Bits.BITS_64) {
      throw new IllegalStateException(
          "Internal error: Only 64-bit floats are currently supported.");
    }
  }

  @Override
  public boolean canApply(StorageType storageType) {
    return storageType instanceof IntegerType
        || storageType instanceof BigDecimalType
        || storageType instanceof BigIntegerType
        || storageType instanceof BooleanType
        || storageType instanceof NullType
        || storageType instanceof AnyObjectType;
  }

  @Override
  public ColumnStorage<Double> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof ColumnLongStorage longStorage) {
      return convertLongStorage(longStorage, problemAggregator);
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage, problemAggregator);
    } else if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return convertBigDecimalStorage(bigDecimalStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType
        || storage.getType() instanceof NullType) {
      return castFromObject(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Float.");
    }
  }

  /** Specialised innerLoop so that we can avoid boxing. */
  static ColumnStorage<Double> innerLoop(
      BuilderForDouble builder,
      ColumnStorage<?> storage,
      ObjLongConsumer<BuilderForDouble> converter) {
    Context context = Context.getCurrent();

    long n = storage.getSize();
    for (long i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        converter.accept(builder, i);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private ColumnStorage<Double> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, storage.getSize(), problemAggregator),
        storage,
        (builder, i) -> {
          Object o = storage.getItemBoxed(i);

          if (NumericConverter.isCoercibleToLong(o)) {
            builder.appendLong(NumericConverter.coerceToLong(o));
          } else if (NumericConverter.isFloatLike(o)) {
            builder.appendDouble(NumericConverter.coerceToDouble(o));
          } else {
            switch (o) {
              case Boolean b -> builder.appendDouble(booleanAsDouble(b));
              case BigInteger bigInteger -> builder.append(bigInteger);
              case BigDecimal bigDecimal -> builder.append(bigDecimal);
              default -> {
                problemAggregator.reportConversionFailure(o);
                builder.appendNulls(1);
              }
            }
          }
        });
  }

  private ColumnStorage<Double> convertLongStorage(
      ColumnLongStorage longStorage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, longStorage.getSize(), problemAggregator),
        longStorage,
        (builder, i) -> {
          long value = longStorage.getItemAsLong(i);
          builder.appendLong(value);
        });
  }

  private ColumnStorage<Double> convertBoolStorage(
      ColumnBooleanStorage boolStorage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, boolStorage.getSize(), problemAggregator),
        boolStorage,
        (builder, i) -> {
          boolean value = boolStorage.getItemAsBoolean(i);
          builder.appendDouble(booleanAsDouble(value));
        });
  }

  private static double booleanAsDouble(boolean value) {
    return value ? 1.0 : 0.0;
  }

  private ColumnStorage<Double> convertBigIntegerStorage(
      ColumnStorage<BigInteger> storage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, storage.getSize(), problemAggregator),
        storage,
        (builder, i) -> builder.append(storage.getItemBoxed(i)));
  }

  private ColumnStorage<Double> convertBigDecimalStorage(
      ColumnStorage<BigDecimal> storage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, storage.getSize(), problemAggregator),
        storage,
        (builder, i) -> builder.append(storage.getItemBoxed(i)));
  }
}
