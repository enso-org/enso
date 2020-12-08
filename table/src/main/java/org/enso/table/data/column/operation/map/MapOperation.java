package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

public abstract class MapOperation<I extends Storage> {
  private final String name;

  public MapOperation(String name) {
    this.name = name;
  }

  public abstract Storage runMap(I storage, Object arg);

  public abstract Storage runZip(I storage, Storage arg);

  public String getName() {
    return name;
  }
}
