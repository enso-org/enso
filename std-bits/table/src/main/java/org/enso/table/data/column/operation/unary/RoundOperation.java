package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Function;
import org.enso.base.numeric.Decimal_Utils;
import org.enso.polyglot.common_utils.Core_Math_Utils;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.operation.NumericColumnAdapter;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.UnaryOperationNumeric;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public class RoundOperation<T, R> extends UnaryOperationNumeric<T, R> {
  /** Minimum value for the `n` parameter to `roundLong`. */
  private static final long ROUND_MIN_LONG = -99999999999999L;

  /** Maximum value for the `n` parameter to `roundLong`. */
  private static final long ROUND_MAX_LONG = 99999999999999L;

  /** Error message for out-of-range values in `roundLong`. */
  private static final String ROUND_LONG_ERROR =
      "Error: `round` can only accept values between "
          + ROUND_MIN_LONG
          + " and "
          + ROUND_MAX_LONG
          + " (inclusive), but was ";

  @FunctionalInterface
  private interface LongRoundingFunction {
    Long apply(long index, long value, MapOperationProblemAggregator problemAggregator);
  }

  private static LongRoundingFunction roundLong(int decimalPlaces, boolean useBankers) {
    return (index, value, problemAggregator) -> {
      if (value < ROUND_MIN_LONG || value > ROUND_MAX_LONG) {
        String message = ROUND_LONG_ERROR + value;
        problemAggregator.reportIllegalArgumentError(message, index);
        return null;
      }

      return Core_Math_Utils.roundLong(value, decimalPlaces, useBankers);
    };
  }

  @FunctionalInterface
  private interface DoubleRoundingFunction<R> {
    R apply(long index, double value, MapOperationProblemAggregator problemAggregator);
  }

  private static DoubleRoundingFunction<Double> roundDouble(int decimalPlaces, boolean useBankers) {
    return (index, value, problemAggregator) -> {
      boolean special = Double.isNaN(value) || Double.isInfinite(value);
      if (special) {
        String message = "Value is " + value;
        problemAggregator.reportArithmeticError(message, index);
        return null;
      }

      return Core_Math_Utils.roundDouble(value, decimalPlaces, useBankers);
    };
  }

  private static DoubleRoundingFunction<Long> roundDoubleToLong(
      int decimalPlaces, boolean useBankers) {
    return (index, value, problemAggregator) -> {
      boolean special = Double.isNaN(value) || Double.isInfinite(value);
      if (special) {
        String message = "Value is " + value;
        problemAggregator.reportArithmeticError(message, index);
        return null;
      }

      return (long) Core_Math_Utils.roundDouble(value, decimalPlaces, useBankers);
    };
  }

  private static Function<BigDecimal, BigDecimal> roundBigDecimal(
      int decimalPlaces, boolean useBankers) {
    return value -> Decimal_Utils.round(value, decimalPlaces, useBankers);
  }

  private static Function<BigDecimal, BigInteger> roundBigDecimalToBigInteger(
      int decimalPlaces, boolean useBankers) {
    return value -> Decimal_Utils.round(value, decimalPlaces, useBankers).toBigInteger();
  }

  private static Function<BigInteger, BigInteger> roundBigInteger(
      int decimalPlaces, boolean useBankers) {
    return value ->
        Decimal_Utils.round(new BigDecimal(value), decimalPlaces, useBankers).toBigInteger();
  }

  public static UnaryOperation create(Column left, long decimalPlacesLong, boolean useBankers) {
    int decimalPlaces = Math.toIntExact(decimalPlacesLong);
    var storage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (storage.getType()) {
      case NullType nt ->
          decimalPlaces <= 0
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
      case BigDecimalType bd ->
          decimalPlaces <= 0
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
      case BigIntegerType bi ->
          decimalPlaces >= 0
              ? UnaryOperation.IDENTITY
              : new RoundOperation<>(
                  NumericColumnAdapter.BigIntegerColumnAdapter.INSTANCE,
                  BigIntegerType.INSTANCE,
                  roundBigInteger(decimalPlaces, useBankers));
      case IntegerType lt ->
          decimalPlaces >= 0
              ? UnaryOperation.IDENTITY
              : createForLong(lt, roundLong(decimalPlaces, useBankers));
      case FloatType ft ->
          decimalPlaces <= 0
              ? createForDouble(IntegerType.INT_64, roundDoubleToLong(decimalPlaces, useBankers))
              : createForDouble(FloatType.FLOAT_64, roundDouble(decimalPlaces, useBankers));
      default ->
          throw new UnsupportedOperationException(
              "Unsupported storage type for round operation: " + storage.getType());
    };
  }

  private static RoundOperation<Long, Long> createForLong(
      StorageType<Long> returnType, LongRoundingFunction longFunction) {
    return new RoundOperation<>(NumericColumnAdapter.LongColumnAdapter.INSTANCE, returnType, null) {
      @Override
      protected Long doSingle(
          long index, Long value, MapOperationProblemAggregator problemAggregator) {
        return longFunction.apply(index, value, problemAggregator);
      }

      @Override
      protected void doSingleSpecializedLong(
          BuilderForType<Long> builder,
          long index,
          long value,
          boolean isNothing,
          MapOperationProblemAggregator problemAggregator) {
        builder.append(longFunction.apply(index, value, problemAggregator));
      }
    };
  }

  private static <R> RoundOperation<Double, R> createForDouble(
      StorageType<R> returnType, DoubleRoundingFunction<R> doubleFunction) {
    return new RoundOperation<>(
        NumericColumnAdapter.DoubleColumnAdapter.INSTANCE, returnType, null) {
      @Override
      protected R doSingle(
          long index, Double value, MapOperationProblemAggregator problemAggregator) {
        return doubleFunction.apply(index, value, problemAggregator);
      }

      @Override
      protected void doSingleSpecializedDouble(
          BuilderForType<R> builder,
          long index,
          double value,
          boolean isNothing,
          MapOperationProblemAggregator problemAggregator) {
        builder.append(doubleFunction.apply(index, value, problemAggregator));
      }
    };
  }

  private final Function<T, R> roundingFunction;

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
    throw new IllegalStateException(
        "No specialized double consumer defined. This is likely a bug.");
  }

  @Override
  protected void doSingleSpecializedLong(
      BuilderForType<R> builder,
      long index,
      long value,
      boolean isNothing,
      MapOperationProblemAggregator problemAggregator) {
    throw new IllegalStateException("No specialized long consumer defined. This is likely a bug.");
  }

  @Override
  protected R doSingle(long index, T value, MapOperationProblemAggregator problemAggregator) {
    return this.roundingFunction.apply(value);
  }
}
