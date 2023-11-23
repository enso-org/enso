package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

/**
 * A representation of a map-like operation that can be performed on given storage types.
 *
 * @param <I> the supported storage type.
 */
public abstract class TernaryMapOperation<T, I extends Storage<? super T>> {
  private final String name;

  /**
   * Creates a new operation with the given name.
   *
   * @param name the operation name
   */
  public TernaryMapOperation(String name) {
    this.name = name;
  }

  /**
   * Run the operation in map mode - combining every row of the storage with a scalar argument.
   *
   * @param storage the storage to run operation on
   * @param arg0 the first argument passed to the operation
   * @param arg1 the first argument passed to the operation
   * @param problemAggregator the aggregator allowing to report computation problems
   * @return the result of running the operation
   */
  public abstract Storage<?> runTernaryMap(
      I storage, Object arg0, Object arg1, MapOperationProblemAggregator problemAggregator);

  /** @return the name of this operation */
  public String getName() {
    return name;
  }
}
