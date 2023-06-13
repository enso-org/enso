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
public class MapOpStorage<T, S extends Storage<? super T>> {
  private final Map<String, MapOperation<T, S>> ops = new HashMap<>();

  protected MapOperation<? super T, ? super S> getOp(String name) {
    return ops.get(name);
  }

  /**
   * Checks if an operation is supported by this set.
   *
   * @param n the operation name
   * @return whether the operation is supported
   */
  public boolean isSupported(String n) {
    return ops.get(n) != null;
  }

  /**
   * Runs the specified operation in map node. This can only be called if {@link
   * #isSupported(String)} returns true, the behavior is unspecified otherwise.
   *
   * @param n the operation name
   * @param storage the storage to run operation on
   * @param arg the argument to pass to the operation
   * @param problemBuilder the builder allowing to report computation problems
   * @return the result of running the operation
   */
  public Storage<?> runMap(
      String n, S storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    return ops.get(n).runMap(storage, arg, problemBuilder);
  }

  /**
   * Runs the specified operation in zip node. This can only be called if {@link
   * #isSupported(String)} returns true, the behavior is unspecified otherwise.
   *
   * @param n the operation name
   * @param storage the storage to run operation on
   * @param arg the storage containing operation arguments
   * @param problemBuilder the builder allowing to report computation problems
   * @return the result of running the operation
   */
  public Storage<?> runZip(
      String n, S storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    return ops.get(n).runZip(storage, arg, problemBuilder);
  }

  /**
   * Adds a new operation to this set.
   *
   * @param op the operation to add
   * @return this operation set
   */
  public MapOpStorage<T, S> add(MapOperation<T, S> op) {
    ops.put(op.getName(), op);
    return this;
  }

  /**
   * Creates a child set, containing all the operations defined in this, that can be extended
   * independently.
   *
   * @param <U> the desired result type
   * @return a child of this storage
   */
  public <U extends T> MapOpStorage<U, S> makeChild() {
    return new ChildStorage<>(this);
  }

  private static class ChildStorage<T, S extends Storage<? super T>> extends MapOpStorage<T, S> {
    private final MapOpStorage<? super T, ? super S> parent;

    private ChildStorage(MapOpStorage<? super T, ? super S> parent) {
      this.parent = parent;
    }

    @Override
    protected MapOperation<? super T, ? super S> getOp(String name) {
      MapOperation<? super T, ? super S> local = super.getOp(name);
      if (local == null) return parent.getOp(name);
      return local;
    }

    @Override
    public boolean isSupported(String n) {
      return super.isSupported(n) || parent.isSupported(n);
    }

    @Override
    public Storage<?> runMap(
        String n, S storage, Object arg, MapOperationProblemBuilder problemBuilder) {
      return getOp(n).runMap(storage, arg, problemBuilder);
    }

    @Override
    public Storage<?> runZip(
        String n, S storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
      return getOp(n).runZip(storage, arg, problemBuilder);
    }
  }
}
