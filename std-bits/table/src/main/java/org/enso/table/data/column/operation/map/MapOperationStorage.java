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
  private final Map<String, UnaryMapOperation<T, S>> unaryOps = new HashMap<>();
  private final Map<String, BinaryMapOperation<T, S>> binaryOps = new HashMap<>();
  private final Map<String, TernaryMapOperation<T, S>> ternaryOps = new HashMap<>();

  /**
   * Checks if a unary operation is supported by this set.
   *
   * @param n the operation name
   * @return whether the operation is supported
   */
  public boolean isSupportedUnary(String n) {
    return n != null && unaryOps.get(n) != null;
  }

  /**
   * Runs the specified unary operation in map node.
   *
   * @param n the operation name
   * @param storage the storage to run operation on
   * @param problemBuilder the builder allowing to report computation problems
   * @return the result of running the operation
   */
  public Storage<?> runUnaryMap(String n, S storage, MapOperationProblemBuilder problemBuilder) {
    if (!isSupportedUnary(n)) {
      throw new IllegalStateException(
          "Requested vectorized unary operation " + n + ", but no such operation is known.");
    }
    return unaryOps.get(n).runUnaryMap(storage, problemBuilder);
  }

  /**
   * Checks if a binary operation is supported by this set.
   *
   * @param n the operation name
   * @return whether the operation is supported
   */
  public boolean isSupportedBinary(String n) {
    return n != null && binaryOps.get(n) != null;
  }

  /**
   * Runs the specified operation in map node.
   *
   * @param n the operation name
   * @param storage the storage to run operation on
   * @param arg the argument to pass to the operation
   * @param problemBuilder the builder allowing to report computation problems
   * @return the result of running the operation
   */
  public Storage<?> runBinaryMap(
      String n, S storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    if (!isSupportedBinary(n)) {
      throw new IllegalStateException(
          "Requested vectorized binary operation " + n + ", but no such operation is known.");
    }
    return binaryOps.get(n).runBinaryMap(storage, arg, problemBuilder);
  }

  /**
   * Checks if a ternary operation is supported by this set.
   *
   * @param n the operation name
   * @return whether the operation is supported
   */
  public boolean isSupportedTernary(String n) {
    return n != null && ternaryOps.get(n) != null;
  }

  /**
   * Runs the specified operation in map node.
   *
   * @param n the operation name
   * @param storage the storage to run operation on
   * @param arg0 the first argument to pass to the operation
   * @param arg1 the second argument to pass to the operation
   * @param problemBuilder the builder allowing to report computation problems
   * @return the result of running the operation
   */
  public Storage<?> runTernaryMap(
      String n, S storage, Object arg0, Object arg1, MapOperationProblemBuilder problemBuilder) {
    if (!isSupportedTernary(n)) {
      throw new IllegalStateException(
          "Requested vectorized ternary operation " + n + ", but no such operation is known.");
    }
    return ternaryOps.get(n).runTernaryMap(storage, arg0, arg1, problemBuilder);
  }

  /**
   * Runs the specified operation in zip node.
   *
   * @param n the operation name
   * @param storage the storage to run operation on
   * @param arg the storage containing operation arguments
   * @param problemBuilder the builder allowing to report computation problems
   * @return the result of running the operation
   */
  public Storage<?> runZip(
      String n, S storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    if (!isSupportedBinary(n)) {
      throw new IllegalStateException(
          "Requested vectorized binary operation " + n + ", but no such operation is known.");
    }

    var operation = binaryOps.get(n);

    if (operation.reliesOnSpecializedStorage()) {
      // We try to get the right-hand side argument as specific as possible, so that our operation
      // will know how to deal with it.
      arg = arg.tryGettingMoreSpecializedStorage();
    }
    return operation.runZip(storage, arg, problemBuilder);
  }

  /**
   * Adds a new operation to this set.
   *
   * @param op the operation to add
   * @return this operation set
   */
  public MapOperationStorage<T, S> add(UnaryMapOperation<T, S> op) {
    unaryOps.put(op.getName(), op);
    return this;
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

  /**
   * Adds a new operation to this set.
   *
   * @param op the operation to add
   * @return this operation set
   */
  public MapOperationStorage<T, S> add(TernaryMapOperation<T, S> op) {
    ternaryOps.put(op.getName(), op);
    return this;
  }
}
