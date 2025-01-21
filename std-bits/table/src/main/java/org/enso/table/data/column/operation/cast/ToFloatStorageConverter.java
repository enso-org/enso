package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.ObjLongConsumer;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForDouble;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.FloatType;
import org.graalvm.polyglot.Context;

public class ToFloatStorageConverter implements StorageConverter<Double> {
  public ToFloatStorageConverter(FloatType targetType) {
    if (targetType.bits() != Bits.BITS_64) {
      throw new IllegalStateException(
          "Internal error: Only 64-bit floats are currently supported.");
    }
  }

  @Override
  public Storage<Double> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof DoubleStorage doubleStorage) {
      return doubleStorage;
    } else if (storage instanceof AbstractLongStorage longStorage) {
      return convertLongStorage(longStorage, problemAggregator);
    } else if (storage instanceof BoolStorage boolStorage) {
      return convertBoolStorage(boolStorage, problemAggregator);
    } else if (storage instanceof BigIntegerStorage bigIntegerStorage) {
      return convertBigIntegerStorage(bigIntegerStorage, problemAggregator);
    } else if (storage instanceof BigDecimalStorage bigDecimalStorage) {
      return convertBigDecimalStorage(bigDecimalStorage, problemAggregator);
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Float.");
    }
  }

  /** Specialised innerLoop so that we can avoid boxing. */
  static Storage<Double> innerLoop(
      BuilderForDouble builder,
      ColumnStorage storage,
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

  private Storage<Double> castFromMixed(
      ColumnStorage mixedStorage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, mixedStorage.getSize(), problemAggregator),
        mixedStorage,
        (builder, i) -> {
          Object o = mixedStorage.getItemAsObject(i);

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

  private Storage<Double> convertLongStorage(
      ColumnLongStorage longStorage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, longStorage.getSize(), problemAggregator),
        longStorage,
        (builder, i) -> {
          long value = longStorage.get(i);
          builder.appendLong(value);
        });
  }

  private Storage<Double> convertBoolStorage(
      ColumnBooleanStorage boolStorage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, boolStorage.getSize(), problemAggregator),
        boolStorage,
        (builder, i) -> {
          boolean value = boolStorage.get(i);
          builder.appendDouble(booleanAsDouble(value));
        });
  }

  private static double booleanAsDouble(boolean value) {
    return value ? 1.0 : 0.0;
  }

  private Storage<Double> convertBigIntegerStorage(
      Storage<BigInteger> storage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, storage.size(), problemAggregator),
        storage,
        (builder, i) -> builder.append(storage.getItemBoxed((int) i)));
  }

  private Storage<Double> convertBigDecimalStorage(
      Storage<BigDecimal> storage, CastProblemAggregator problemAggregator) {
    return innerLoop(
        Builder.getForDouble(FloatType.FLOAT_64, storage.size(), problemAggregator),
        storage,
        (builder, i) -> builder.append(storage.getItemBoxed((int) i)));
  }
}
