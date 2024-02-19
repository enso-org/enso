package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.LongBuilder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;

/** An abstract base class for unary operations returning a long column. */
abstract class AbstractUnaryLongOperation extends AbstractUnaryOperation {
  private final IntegerType returnType;

  /**
   * Creates a new AbstractUnaryOperation.
   *
   * @param name the name of the operation
   * @param nothingUnchanged whether the operation should return nothing if the input is nothing
   * @param returnType the type of the column that will be returned
   */
  protected AbstractUnaryLongOperation(
      String name, boolean nothingUnchanged, IntegerType returnType) {

    super(name, nothingUnchanged);
    this.returnType = returnType;
  }

  @Override
  protected LongBuilder createBuilder(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    if (storage.getSize() > Integer.MAX_VALUE) {
      throw new IllegalArgumentException(
          "Cannot currently operate on columns larger than " + Integer.MAX_VALUE + ".");
    }
    return LongBuilder.createLongBuilder((int) storage.getSize(), returnType, problemAggregator);
  }

  @Override
  protected final void applyObjectRow(
      Object value, Builder builder, MapOperationProblemAggregator problemAggregator) {
    applyObjectRow(value, (LongBuilder) builder, problemAggregator);
  }

  protected abstract void applyObjectRow(
      Object value, LongBuilder builder, MapOperationProblemAggregator problemAggregator);
}
