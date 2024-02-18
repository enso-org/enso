package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;

public class IsNaNOperation extends AbstractUnaryBooleanOperation {
  public static final UnaryOperation INSTANCE = new IsNaNOperation();

  private IsNaNOperation() {
    super(UnaryOperation.IS_NAN, true);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType().isNumeric();
  }

  @Override
  protected void applyLong(ColumnLongStorage longStorage, Builder builder, MapOperationProblemAggregator problemAggregator) {
    var boolBuilder = (BoolBuilder)builder;
    UnaryOperation.applyOverLongStorage(longStorage, true, builder, (isNothing, value) -> boolBuilder.appendBoolean(false));
  }

  @Override
  protected void applyDouble(ColumnDoubleStorage doubleStorage, Builder builder, MapOperationProblemAggregator problemAggregator) {
    var boolBuilder = (BoolBuilder)builder;
    UnaryOperation.applyOverDoubleStorage(doubleStorage, true, builder, (isNothing, value) -> boolBuilder.appendBoolean(Double.isNaN(value)));
  }

  @Override
  protected void applyObjectRow(Object value, BoolBuilder builder, MapOperationProblemAggregator problemAggregator) {
    // Null handled by base class
    switch (value) {
      case Double d -> builder.appendBoolean(Double.isNaN(d));
      case Float f -> builder.appendBoolean(Float.isNaN(f));
      case Number ignored -> builder.appendBoolean(false);
      default -> throw new IllegalArgumentException("Unsupported type: "+value.getClass() + " (expected numeric type).");
    }
  }
}


