package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public class IsInfiniteOperation extends AbstractUnaryBooleanOperation {
  public static final UnaryOperation INSTANCE = new IsInfiniteOperation();

  private IsInfiniteOperation() {
    super(UnaryOperation.IS_NAN, true);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType().isNumeric();
  }

  @Override
  protected void applyLong(ColumnLongStorage longStorage, Builder builder, ProblemAggregator problemAggregator) {
    var boolBuilder = (BoolBuilder)builder;
    UnaryOperation.applyOverLongStorage(longStorage, true, builder, (isNothing, value) -> boolBuilder.appendBoolean(false));
  }

  @Override
  protected void applyDouble(ColumnDoubleStorage doubleStorage, Builder builder, ProblemAggregator problemAggregator) {
    var boolBuilder = (BoolBuilder)builder;
    UnaryOperation.applyOverDoubleStorage(doubleStorage, true, builder, (isNothing, value) -> boolBuilder.appendBoolean(Double.isInfinite(value)));
  }

  @Override
  protected void applyObjectRow(Object value, BoolBuilder builder, ProblemAggregator problemAggregator) {
    // Null handled by base class
    switch (value) {
      case Double d -> builder.appendBoolean(Double.isInfinite(d));
      case Float f -> builder.appendBoolean(Float.isInfinite(f));
      case Number ignored -> builder.appendBoolean(false);
      default -> throw new IllegalArgumentException("Unsupported type: "+value.getClass() + " (expected numeric type).");
    }
  }
}
