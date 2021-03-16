package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

import java.util.HashMap;
import java.util.Map;

/**
 * Stores map-like operations that can be performed on a given type.
 *
 * @param <T> the storage type handled by these operations.
 */
public class MapOpStorage<T extends Storage> {
  private final Map<String, MapOperation<T>> ops = new HashMap<>();

  protected MapOperation<? super T> getOp(String name) {
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
   * @return the result of running the operation
   */
  public Storage runMap(String n, T storage, Object arg) {
    return ops.get(n).runMap(storage, arg);
  }

  /**
   * Runs the specified operation in zip node. This can only be called if {@link
   * #isSupported(String)} returns true, the behavior is unspecified otherwise.
   *
   * @param n the operation name
   * @param storage the storage to run operation on
   * @param arg the storage containing operation arguments
   * @return the result of running the operation
   */
  public Storage runZip(String n, T storage, Storage arg) {
    return ops.get(n).runZip(storage, arg);
  }

  /**
   * Adds a new operation to this set.
   *
   * @param op the operation to add
   * @return this operation set
   */
  public MapOpStorage<T> add(MapOperation<T> op) {
    ops.put(op.getName(), op);
    return this;
  }

  /**
   * Creates a child set, containing all the operations defined in this, that can be extended
   * independently.
   *
   * @param <S> the desired result type
   * @return a child of this storage
   */
  public <S extends T> MapOpStorage<S> makeChild() {
    return new ChildStorage<>(this);
  }

  private static class ChildStorage<T extends Storage> extends MapOpStorage<T> {
    private final MapOpStorage<? super T> parent;

    private ChildStorage(MapOpStorage<? super T> parent) {
      this.parent = parent;
    }

    @Override
    protected MapOperation<? super T> getOp(String name) {
      MapOperation<? super T> local = super.getOp(name);
      if (local == null) return parent.getOp(name);
      return local;
    }

    @Override
    public boolean isSupported(String n) {
      return super.isSupported(n) || parent.isSupported(n);
    }

    @Override
    public Storage runMap(String n, T storage, Object arg) {
      return getOp(n).runMap(storage, arg);
    }

    @Override
    public Storage runZip(String n, T storage, Storage arg) {
      return getOp(n).runZip(storage, arg);
    }
  }
}
