package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.Storage;

public abstract class Builder {
  public abstract void append(Object o);

  public abstract int getCurrentSize();

  public abstract Storage seal();
}
