package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.DoubleFunction;
import java.util.function.Function;
import java.util.function.LongFunction;
import org.enso.base.numeric.Decimal_Utils;
import org.enso.polyglot.common_utils.Core_Math_Utils;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.NumericColumnAdapter;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.UnaryOperationNumeric;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;

public final class RoundOperation<T, R> extends UnaryOperationNumeric<T, R> {
  private static LongFunction<Long> roundLong(long decimalPlaces, boolean useBankers) {
    return value -> Core_Math_Utils.roundLong(value, (int) decimalPlaces, useBankers);
  }

  private static DoubleFunction<Double> roundDouble(long decimalPlaces, boolean useBankers) {
    return value -> Core_Math_Utils.roundDouble(value, (int) decimalPlaces, useBankers);
  }

  private static DoubleFunction<Long> roundDoubleToLong(long decimalPlaces, boolean useBankers) {
    return value -> (long) Core_Math_Utils.roundDouble(value, (int) decimalPlaces, useBankers);
  }

  private static Function<BigDecimal, BigDecimal> roundBigDecimal(
      long decimalPlaces, boolean useBankers) {
    return value -> Decimal_Utils.round(value, (int) decimalPlaces, useBankers);
  }

  private static Function<BigDecimal, BigInteger> roundBigDecimalToBigInteger(
      long decimalPlaces, boolean useBankers) {
    return value -> Decimal_Utils.round(value, (int) decimalPlaces, useBankers).toBigInteger();
  }

  private static Function<BigInteger, BigInteger> roundBigInteger(
      long decimalPlaces, boolean useBankers) {
    return value ->
        Decimal_Utils.round(new BigDecimal(value), (int) decimalPlaces, useBankers).toBigInteger();
  }

  public static UnaryOperation create(Column left, long decimalPlaces, boolean useBankers) {
    var storage = BinaryOperation.getInferredStorage(left);
    return switch (storage.getType()) {
      case NullType nt -> decimalPlaces <= 0
          ? new RoundOperation<>(
              NumericColumnAdapter.DoubleColumnAdapter.INSTANCE,
              true,
              IntegerType.INT_64,
              v -> null)
          : new RoundOperation<>(
              NumericColumnAdapter.DoubleColumnAdapter.INSTANCE,
              true,
              FloatType.FLOAT_64,
              v -> null);
      case BigDecimalType bd -> decimalPlaces <= 0
          ? new RoundOperation<>(
              NumericColumnAdapter.BigDecimalColumnAdapter.INSTANCE,
              false,
              BigIntegerType.INSTANCE,
              roundBigDecimalToBigInteger(decimalPlaces, useBankers))
          : new RoundOperation<>(
              NumericColumnAdapter.BigDecimalColumnAdapter.INSTANCE,
              false,
              BigDecimalType.INSTANCE,
              roundBigDecimal(decimalPlaces, useBankers));
      case BigIntegerType bi -> decimalPlaces >= 0
          ? UnaryOperation.IDENTITY
          : new RoundOperation<>(
              NumericColumnAdapter.BigIntegerColumnAdapter.INSTANCE,
              BigIntegerType.INSTANCE,
              roundBigInteger(decimalPlaces, useBankers));
      case IntegerType lt -> decimalPlaces >= 0
          ? UnaryOperation.IDENTITY
          : createForLong(lt, roundLong(decimalPlaces, useBankers));
      case FloatType ft -> decimalPlaces <= 0
          ? createForDouble(IntegerType.INT_64, roundDoubleToLong(decimalPlaces, useBankers))
          : createForDouble(FloatType.FLOAT_64, roundDouble(decimalPlaces, useBankers));
      default -> throw new UnsupportedOperationException(
          "Unsupported storage type for round operation: " + storage.getType());
    };
  }

  private static <R> RoundOperation<Long, R> createForLong(
      StorageType<R> returnType, LongFunction<R> longFunction) {
    Function<Long, R> function = value -> (value == null ? null : longFunction.apply(value));
    SpecializedLongConsumer<R> consumer =
        (builder, index, value, isNothing, problemAggregator) -> {
          if (isNothing) {
            builder.appendNulls(1);
          } else {
            builder.append(longFunction.apply(value));
          }
        };
    var initialOperation =
        new RoundOperation<>(NumericColumnAdapter.LongColumnAdapter.INSTANCE, returnType, function);
    initialOperation.specializedLongConsumer = consumer;
    return initialOperation;
  }

  private static <R> RoundOperation<Double, R> createForDouble(
      StorageType<R> returnType, DoubleFunction<R> doubleFunction) {
    Function<Double, R> function = value -> (value == null ? null : doubleFunction.apply(value));
    SpecializedDoubleConsumer<R> consumer =
        (builder, index, value, isNothing, problemAggregator) -> {
          if (isNothing) {
            builder.appendNulls(1);
          } else {
            builder.append(doubleFunction.apply(value));
          }
        };
    var initialOperation =
        new RoundOperation<>(
            NumericColumnAdapter.DoubleColumnAdapter.INSTANCE, returnType, function);
    initialOperation.specializedDoubleConsumer = consumer;
    return initialOperation;
  }

  private final Function<T, R> roundingFunction;
  private SpecializedLongConsumer<R> specializedLongConsumer = null;
  private SpecializedDoubleConsumer<R> specializedDoubleConsumer = null;

  private RoundOperation(
      NumericColumnAdapter<T> adapter, StorageType<R> returnType, Function<T, R> roundingFunction) {
    this(adapter, true, returnType, roundingFunction);
  }

  private RoundOperation(
      NumericColumnAdapter<T> adapter,
      boolean allowNullType,
      StorageType<R> returnType,
      Function<T, R> roundingFunction) {
    super(adapter, allowNullType, true, returnType);
    this.roundingFunction = roundingFunction;
  }

  @Override
  public String getName() {
    return "round";
  }

  @Override
  protected void doSingleSpecializedDouble(
      BuilderForType<R> builder,
      long index,
      double value,
      boolean isNothing,
      MapOperationProblemAggregator problemAggregator) {
    // Nulls preserved so don't need to check for isNothing
    if (specializedDoubleConsumer != null) {
      specializedDoubleConsumer.accept(builder, index, value, isNothing, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No specialized double consumer defined. This is likely a bug.");
    }
  }

  @Override
  protected void doSingleSpecializedLong(
      BuilderForType<R> builder,
      long index,
      long value,
      boolean isNothing,
      MapOperationProblemAggregator problemAggregator) {
    // Nulls preserved so don't need to check for isNothing
    if (specializedLongConsumer != null) {
      specializedLongConsumer.accept(builder, index, value, isNothing, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No specialized long consumer defined. This is likely a bug.");
    }
  }

  @Override
  protected R doSingle(long index, T value, MapOperationProblemAggregator problemAggregator) {
    return this.roundingFunction.apply(value);
  }
}
