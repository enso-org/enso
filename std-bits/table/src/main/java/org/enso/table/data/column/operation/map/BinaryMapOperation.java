package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

/**
 * A representation of a map-like operation that can be performed on given storage types.
 *
 * @param <I> the supported storage type.
 */
public abstract class BinaryMapOperation<T, I extends Storage<? super T>> {
  private final String name;

  /**
   * Creates a new operation with the given name.
   *
   * @param name the operation name
   */
  public BinaryMapOperation(String name) {
    this.name = name;
  }

  /**
   * Run the operation in map mode - combining every row of the storage with a scalar argument.
   *
   * @param storage the storage to run operation on
   * @param arg the argument passed to the operation
   * @param problemAggregator the aggregator allowing to report computation problems
   * @return the result of running the operation
   */
  public abstract Storage<?> runBinaryMap(
      I storage, Object arg, MapOperationProblemAggregator problemAggregator);

  /**
   * Run the operation in zip mode - combining corresponding rows of two storages.
   *
   * @param storage the storage to run operation on
   * @param arg the storage providing second arguments to the operation
   * @param problemAggregator the aggregator allowing to report computation problems
   * @return the result of running the operation
   */
  public abstract Storage<?> runZip(
      I storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator);

  /** @return the name of this operation */
  public String getName() {
    return name;
  }

  /**
   * Specifies if the operation relies on specialized storage types.
   *
   * <p>Some operations, e.g. numeric operations, may only work with specialized numeric storages.
   * In this case, the caller will ensure that if a mixed column pretending to be numeric is passed
   * to such an operation, it will first be converted to a specialized type. If a given operation
   * can handle any storage, this may return false to avoid an unnecessary costly conversion.
   */
  public boolean reliesOnSpecializedStorage() {
    return true;
  }
}
