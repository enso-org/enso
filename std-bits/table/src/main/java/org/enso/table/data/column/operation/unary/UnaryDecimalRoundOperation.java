package org.enso.table.data.column.operation.unary;

import java.util.function.DoubleToLongFunction;
import org.enso.table.data.column.builder.LongBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;

public class UnaryDecimalRoundOperation extends AbstractUnaryOperation {
  public static final String CEIL = "ceil";
  public static final UnaryOperation CEIL_INSTANCE =
      new UnaryDecimalRoundOperation(CEIL, d -> (long) Math.ceil(d));

  public static final String FLOOR = "floor";
  public static final UnaryOperation FLOOR_INSTANCE =
      new UnaryDecimalRoundOperation(FLOOR, d -> (long) Math.floor(d));

  public static String TRUNCATE = "truncate";
  public static final UnaryOperation TRUNCATE_INSTANCE =
      new UnaryDecimalRoundOperation(TRUNCATE, d -> (long) d);

  private final DoubleToLongFunction function;

  private UnaryDecimalRoundOperation(String name, DoubleToLongFunction function) {
    super(name, true, IntegerType.INT_64);
    this.function = function;
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType().isNumeric();
  }

  @Override
  public ColumnStorage apply(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof ColumnLongStorage longStorage) {
      // For a long storage, the operation is an identity operation.
      return longStorage;
    }

    return super.apply(storage, problemAggregator);
  }

  @Override
  protected void applyObjectRow(
      Object value, LongBuilder builder, MapOperationProblemAggregator problemAggregator) {
    // Null handled by base class
    switch (value) {
      case Double d -> {
        if (Double.isNaN(d) || Double.isInfinite(d)) {
          String msg = "Value is " + d;
          problemAggregator.reportArithmeticError(msg, builder.getCurrentSize());
          builder.appendNulls(1);
        } else {
          builder.appendLong(function.applyAsLong(d));
        }
      }
      case Float f -> applyObjectRow((double) f, builder, problemAggregator);
      case Number n -> applyObjectRow(n.doubleValue(), builder, problemAggregator);
      default -> throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected numeric type).");
    }
  }
}