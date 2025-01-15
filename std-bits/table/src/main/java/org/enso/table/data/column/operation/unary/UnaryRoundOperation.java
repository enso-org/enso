package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.DoubleToLongFunction;
import java.util.function.Function;
import org.enso.base.numeric.Decimal_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.InferredIntegerBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;

public class UnaryRoundOperation extends AbstractUnaryOperation {
  // Used to determine whether we should use Double or BigDecimal operations.
  // Values outside this range are promoted to BigDecimal operation, because
  // representing their rounded value as a Long might overflow the Long dynamic
  // range.
  public static final double USE_DOUBLE_LIMIT_POSITIVE = 9223372036854775000.0;
  public static final double USE_DOUBLE_LIMIT_NEGATIVE = -9223372036854775000.0;

  public static final String CEIL = "ceil";
  public static final UnaryOperation CEIL_INSTANCE =
      new UnaryRoundOperation(CEIL, d -> (long) Math.ceil(d), Decimal_Utils::ceil);

  public static final String FLOOR = "floor";
  public static final UnaryOperation FLOOR_INSTANCE =
      new UnaryRoundOperation(FLOOR, d -> (long) Math.floor(d), Decimal_Utils::floor);

  public static String TRUNCATE = "truncate";
  public static final UnaryOperation TRUNCATE_INSTANCE =
      new UnaryRoundOperation(TRUNCATE, d -> (long) d, Decimal_Utils::truncate);

  private final DoubleToLongFunction doubleFunction;
  private final Function<BigDecimal, BigInteger> bigDecimalFunction;

  private UnaryRoundOperation(
      String name,
      DoubleToLongFunction doubleFunction,
      Function<BigDecimal, BigInteger> bigDecimalFunction) {
    super(name, true);
    this.doubleFunction = doubleFunction;
    this.bigDecimalFunction = bigDecimalFunction;
  }

  protected Builder createBuilder(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    if (storage.getSize() > Integer.MAX_VALUE) {
      throw new IllegalArgumentException(
          "Cannot currently operate on columns larger than " + Integer.MAX_VALUE + ".");
    }

    return new InferredIntegerBuilder((int) storage.getSize(), problemAggregator);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType().isNumeric();
  }

  @Override
  public ColumnStorage apply(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof ColumnLongStorage || storage instanceof BigIntegerStorage) {
      // For an integral type storage, the operation is an identity operation.
      return storage;
    }

    return super.apply(storage, problemAggregator);
  }

  @Override
  protected void applyObjectRow(
      Object value, Builder builder, MapOperationProblemAggregator problemAggregator) {
    // Null handled by base class
    switch (value) {
      case Double d -> {
        if (Double.isNaN(d) || Double.isInfinite(d)) {
          String msg = "Value is " + d;
          problemAggregator.reportArithmeticError(msg, builder.getCurrentSize());
          builder.appendNulls(1);
        } else if (d > USE_DOUBLE_LIMIT_POSITIVE || d < USE_DOUBLE_LIMIT_NEGATIVE) {
          builder.append(bigDecimalFunction.apply(BigDecimal.valueOf(d)));
        } else {
          builder.append(doubleFunction.applyAsLong(d));
        }
      }
      case Float f -> applyObjectRow((double) f, builder, problemAggregator);
      case BigDecimal bd -> {
        builder.append(bigDecimalFunction.apply(bd));
      }
      case Number n -> applyObjectRow(n.doubleValue(), builder, problemAggregator);
      default -> throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected numeric type).");
    }
  }
}
