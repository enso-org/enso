package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

import java.util.HashMap;
import java.util.Map;

public class MapOpStorage<T extends Storage> {
  private final Map<String, MapOperation<T>> ops = new HashMap<>();

  public boolean isSupported(String n) {
    return ops.get(n) != null;
  }

  public Storage run(String n, T storage, Object arg) {
    return ops.get(n).run(storage, arg);
  }

  public MapOpStorage<T> add(MapOperation<T> op) {
    ops.put(op.getName(), op);
    return this;
  }
}
