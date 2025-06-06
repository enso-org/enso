package org.enso.table.data.column.operation.map;

import java.util.HashMap;
import java.util.Map;
import org.enso.table.data.column.storage.Storage;

/**
 * Stores map-like operations that can be performed on a given type.
 *
 * @param <T> the type of elements stored in the storage
 * @param <S> the storage type handled by these operations.
 */
public class MapOperationStorage<T, S extends Storage<? super T>> {
  private final Map<String, BinaryMapOperation<T, S>> binaryOps = new HashMap<>();

  /**
   * Checks if a binary operation is supported by this set.
   *
   * @param n the operation name
   * @return whether the operation is supported
   */
  private boolean isSupportedBinary(String n) {
    return n != null && binaryOps.get(n) != null;
  }

  /**
   * Runs the specified operation in map node.
   *
   * @param n the operation name
   * @param storage the storage to run operation on
   * @param arg the argument to pass to the operation
   * @param problemAggregator the aggregator allowing to report computation problems
   * @return the result of running the operation
   */
  public Storage<?> runBinaryMap(
      String n, S storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    return isSupportedBinary(n)
        ? binaryOps.get(n).runBinaryMap(storage, arg, problemAggregator)
        : null;
  }

  /**
   * Runs the specified operation in zip node.
   *
   * @param n the operation name
   * @param storage the storage to run operation on
   * @param arg the storage containing operation arguments
   * @param problemAggregator the aggregator allowing to report computation problems
   * @return the result of running the operation
   */
  public Storage<?> runZip(
      String n, S storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    if (!isSupportedBinary(n)) {
      return null;
    }

    var operation = binaryOps.get(n);

    if (operation.reliesOnSpecializedStorage()) {
      // We try to get the right-hand side argument as specific as possible, so that our operation
      // will know how to deal with it.
      arg = arg.tryGettingMoreSpecializedStorage();
    }
    return operation.runZip(storage, arg, problemAggregator);
  }

  /**
   * Adds a new operation to this set.
   *
   * @param op the operation to add
   * @return this operation set
   */
  public MapOperationStorage<T, S> add(BinaryMapOperation<T, S> op) {
    binaryOps.put(op.getName(), op);
    return this;
  }
}
