package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.InferredBuilder;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;

/**
 * An abstract base class for unary operations. This class provides a default implementation for
 * applying the operation to a column storage.
 */
abstract class AbstractUnaryOperation implements UnaryOperation {
  private final String name;
  private final boolean nothingUnchanged;

  /**
   * Creates a new AbstractUnaryOperation.
   *
   * @param name the name of the operation
   * @param nothingUnchanged whether the operation should return nothing if the input is nothing
   */
  protected AbstractUnaryOperation(String name, boolean nothingUnchanged) {
    this.name = name;
    this.nothingUnchanged = nothingUnchanged;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public abstract boolean canApply(ColumnStorage storage);

  @Override
  public ColumnStorage apply(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    var builder = createBuilder(storage, problemAggregator);

    switch (storage) {
      case ColumnBooleanStorage booleanStorage -> applyBoolean(
          booleanStorage, builder, problemAggregator);
      case ColumnLongStorage longStorage -> applyLong(longStorage, builder, problemAggregator);
      case ColumnDoubleStorage doubleStorage -> applyDouble(
          doubleStorage, builder, problemAggregator);
      default -> applyObject(storage, builder, problemAggregator);
    }

    return builder.seal();
  }

  protected Builder createBuilder(
      ColumnStorage storage, MapOperationProblemAggregator problemAggregator) {
    if (storage.getSize() > Integer.MAX_VALUE) {
      throw new IllegalArgumentException(
          "Cannot currently operate on columns larger than " + Integer.MAX_VALUE + ".");
    }

    return new InferredBuilder((int) storage.getSize(), problemAggregator);
  }

  /** Apply the operation to a Boolean Storage. */
  protected void applyBoolean(
      ColumnBooleanStorage booleanStorage,
      Builder builder,
      MapOperationProblemAggregator problemAggregator) {
    applyObject(booleanStorage, builder, problemAggregator);
  }

  /** Apply the operation to a Long Storage. */
  protected void applyLong(
      ColumnLongStorage longStorage,
      Builder builder,
      MapOperationProblemAggregator problemAggregator) {
    applyObject(longStorage, builder, problemAggregator);
  }

  /** Apply the operation to a Double Storage. */
  protected void applyDouble(
      ColumnDoubleStorage doubleStorage,
      Builder builder,
      MapOperationProblemAggregator problemAggregator) {
    applyObject(doubleStorage, builder, problemAggregator);
  }

  /** Apply the operation to an Object Storage. */
  protected void applyObject(
      ColumnStorage objectStorage,
      Builder builder,
      MapOperationProblemAggregator problemAggregator) {
    UnaryOperation.applyOverObjectStorage(
        objectStorage,
        nothingUnchanged,
        builder,
        o -> applyObjectRow(o, builder, problemAggregator));
  }

  protected abstract void applyObjectRow(
      Object value, Builder builder, MapOperationProblemAggregator problemAggregator);
}
