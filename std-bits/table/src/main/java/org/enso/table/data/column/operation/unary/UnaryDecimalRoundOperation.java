package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Function;
import org.enso.base.numeric.Decimal_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.InferredBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;

public class UnaryDecimalRoundOperation extends AbstractUnaryOperation {
  public static final String CEIL = "ceil";
  public static final UnaryOperation CEIL_INSTANCE =
      new UnaryDecimalRoundOperation(CEIL, Decimal_Utils::ceil);

  public static final String FLOOR = "floor";
  public static final UnaryOperation FLOOR_INSTANCE =
      new UnaryDecimalRoundOperation(FLOOR, Decimal_Utils::floor);

  public static String TRUNCATE = "truncate";
  public static final UnaryOperation TRUNCATE_INSTANCE =
      new UnaryDecimalRoundOperation(TRUNCATE, Decimal_Utils::truncate);

  private final Function<BigDecimal, BigInteger> function;

  private UnaryDecimalRoundOperation(String name, Function<BigDecimal, BigInteger> function) {
    super(name, true);
    this.function = function;
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType() instanceof BigDecimalType;
  }

  @Override
  protected final void applyObjectRow(
      Object value, Builder builder, MapOperationProblemAggregator problemAggregator) {
    applyObjectRow(value, (InferredBuilder) builder, problemAggregator);
  }

  protected void applyObjectRow(
      Object value, InferredBuilder builder, MapOperationProblemAggregator problemAggregator) {
    switch (value) {
      case BigDecimal d -> builder.append(function.apply(d));
      default -> throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected decimal).");
    }
  }
}
