package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

public abstract class UnaryMapOperation<I extends Storage> extends MapOperation<I> {
  public UnaryMapOperation(String name) {
    super(name);
  }

  protected abstract Storage run(I storage);

  @Override
  public Storage runMap(I storage, Object arg) {
    return run(storage);
  }

  @Override
  public Storage runZip(I storage, Storage arg) {
    return run(storage);
  }
}
