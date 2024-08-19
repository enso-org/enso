package org.enso.table.data.column.operation.unary;

import java.util.BitSet;
import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.type.AnyObjectType;

public class IsNaNOperation extends AbstractUnaryBooleanOperation {
  public static final String NAME = "is_nan";
  public static final UnaryOperation INSTANCE = new IsNaNOperation();

  private IsNaNOperation() {
    super(NAME, true);
  }

  @Override
  public boolean canApply(ColumnStorage storage) {
    var type = storage.getType();
    // We also allow this operation on Mixed type to facilitate `internal_is_nan` helper.
    return type.isNumeric() || type instanceof AnyObjectType;
  }

  @Override
  public ColumnStorage apply(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof ColumnLongStorage
        && storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      // For a Column of Longs where we have the Nothing map, we can produce result immediately.
      return new BoolStorage(
          new BitSet(), withNothingMap.getIsNothingMap(), (int) storage.getSize(), false);
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
        longStorage, true, builder, (isNothing, value) -> boolBuilder.appendBoolean(false));
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
        (isNothing, value) -> boolBuilder.appendBoolean(Double.isNaN(value)));
  }

  @Override
  protected void applyObjectRow(
      Object value, BoolBuilder builder, MapOperationProblemAggregator problemAggregator) {
    // Null handled by base class
    switch (value) {
      case Double d -> builder.appendBoolean(Double.isNaN(d));
      case Float f -> builder.appendBoolean(Float.isNaN(f));
      default -> builder.appendBoolean(false);
    }
  }
}
