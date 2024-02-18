package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.LongBuilder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.ProblemAggregator;

/**
 * An abstract base class for unary operations returning a long column.
 */
abstract class AbstractUnaryLongOperation extends AbstractUnaryOperation {
  private final IntegerType valueType;
  /**
   * Creates a new AbstractUnaryOperation.
   *
   * @param name             the name of the operation
   * @param nothingUnchanged whether the operation should return nothing if the input is nothing
   */
  protected AbstractUnaryLongOperation(String name, boolean nothingUnchanged, IntegerType valueType) {

    super(name, nothingUnchanged);
    this.valueType = valueType;
  }

  @Override
  protected LongBuilder createBuilder(ColumnStorage storage, ProblemAggregator problemAggregator) {
    if (storage.getSize() > Integer.MAX_VALUE) {
      throw new IllegalArgumentException("Cannot currently operate on columns larger than "+Integer.MAX_VALUE+".");
    }
    return LongBuilder.createLongBuilder((int)storage.getSize(), valueType, problemAggregator);
  }

  @Override
  protected final void applyObjectRow(Object value, Builder builder, ProblemAggregator problemAggregator) {
    applyObjectRow(value, (LongBuilder)builder, problemAggregator);
  }

  protected abstract void applyObjectRow(Object value, LongBuilder builder, ProblemAggregator problemAggregator);
}
