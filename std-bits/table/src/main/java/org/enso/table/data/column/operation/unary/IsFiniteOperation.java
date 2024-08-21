package org.enso.table.data.column.operation.unary;

import java.util.BitSet;
import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.*;

public class IsFiniteOperation extends AbstractUnaryBooleanOperation {
  public static final String NAME = "is_finite";
  public static final UnaryOperation INSTANCE = new IsFiniteOperation();

  private IsFiniteOperation() {
    super(NAME, true);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    return storage.getType().isNumeric();
  }

  @Override
  public ColumnStorage apply(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof ColumnLongStorage
        && storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      // For a Column of Longs where we have the Nothing map, we can produce result immediately.
      return new BoolStorage(
          new BitSet(), withNothingMap.getIsNothingMap(), (int) storage.getSize(), true);
    }

    return super.apply(storage, problemAggregator);
  }

  @Override
  protected void applyLong(
      ColumnLongStorage longStorage,
      Builder builder,
      MapOperationProblemAggregator problemAggregator) {
    var boolBuilder = (BoolBuilder) builder;
    UnaryOperation.applyOverLongStorage(
        longStorage, true, builder, (isNothing, value) -> boolBuilder.appendBoolean(!isNothing));
  }

  @Override
  protected void applyDouble(
      ColumnDoubleStorage doubleStorage,
      Builder builder,
      MapOperationProblemAggregator problemAggregator) {
    var boolBuilder = (BoolBuilder) builder;
    UnaryOperation.applyOverDoubleStorage(
        doubleStorage,
        true,
        builder,
        (isNothing, value) -> boolBuilder.appendBoolean(!isNothing && Double.isFinite(value)));
  }

  @Override
  protected void applyObjectRow(
      Object value, BoolBuilder builder, MapOperationProblemAggregator problemAggregator) {
    // Null handled by base class
    switch (value) {
      case Double d -> builder.appendBoolean(Double.isFinite(d));
      case Float f -> builder.appendBoolean(Float.isFinite(f));
      case Number ignored -> builder.appendBoolean(true);
      default -> throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected numeric type).");
    }
  }
}
