package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

/**
 * A map-like operation that ignores its second argument
 *
 * @param <I> the supported storage type
 */
public abstract class UnaryMapOperation<T, I extends Storage<T>> extends MapOperation<T, I> {
  public UnaryMapOperation(String name) {
    super(name);
  }

  protected abstract Storage<?> run(I storage);

  @Override
  public Storage<?> runMap(I storage, Object arg) {
    return run(storage);
  }

  @Override
  public Storage<?> runZip(I storage, Storage<?> arg) {
    return run(storage);
  }
}
