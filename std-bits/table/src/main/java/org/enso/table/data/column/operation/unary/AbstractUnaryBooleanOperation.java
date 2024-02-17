package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

/**
 * An abstract base class for unary operations returning a boolean column.
 */
abstract class AbstractUnaryBooleanOperation extends AbstractUnaryOperation {
  /**
   * Creates a new AbstractUnaryOperation.
   *
   * @param name             the name of the operation
   * @param nothingUnchanged whether the operation should return nothing if the input is nothing
   */
  protected AbstractUnaryBooleanOperation(String name, boolean nothingUnchanged) {
    super(name, nothingUnchanged);
  }

  @Override
  protected BoolBuilder createBuilder(ColumnStorage storage, ProblemAggregator problemAggregator) {
    if (storage.getSize() > Integer.MAX_VALUE) {
      throw new IllegalArgumentException(STR."Cannot currently operate on columns larger than \{Integer.MAX_VALUE}.");
    }
    return new BoolBuilder((int)storage.getSize());
  }
}
