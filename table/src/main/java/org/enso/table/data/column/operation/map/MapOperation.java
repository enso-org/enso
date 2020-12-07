package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

public abstract class MapOperation<I extends Storage> {
  private final String name;

  public MapOperation(String name) {
    this.name = name;
  }

  public abstract Storage run(I storage, Object arg);

  public String getName() {
    return name;
  }
}
