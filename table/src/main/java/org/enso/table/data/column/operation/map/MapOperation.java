package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

/**
 * A representation of a map-like operation that can be performed on given storage types.
 *
 * @param <I> the supported storage type.
 */
public abstract class MapOperation<I extends Storage> {
  private final String name;

  /**
   * Creates a new operation with the given name.
   *
   * @param name the operation name
   */
  public MapOperation(String name) {
    this.name = name;
  }

  /**
   * Run the operation in map mode
   *
   * @param storage the storage to run operation on
   * @param arg the argument passed to the operation
   * @return the result of running the operation
   */
  public abstract Storage runMap(I storage, Object arg);

  /**
   * Run the operation in zip mode
   *
   * @param storage the storage to run operation on
   * @param arg the storage providing second arguments to the operation
   * @return the result of running the operation
   */
  public abstract Storage runZip(I storage, Storage arg);

  /** @return the name of this operation */
  public String getName() {
    return name;
  }
}
