package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.Storage;

import java.util.HashMap;
import java.util.Map;

public class MapOpStorage<T extends Storage> {
  private final Map<String, MapOperation<T>> ops = new HashMap<>();

  protected MapOperation<? super T> getOp(String name) {
    return ops.get(name);
  }

  public boolean isSupported(String n) {
    return ops.get(n) != null;
  }

  public Storage runMap(String n, T storage, Object arg) {
    return ops.get(n).runMap(storage, arg);
  }

  public Storage runZip(String n, T storage, Storage arg) {
    return ops.get(n).runZip(storage, arg);
  }

  public MapOpStorage<T> add(MapOperation<T> op) {
    ops.put(op.getName(), op);
    return this;
  }

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
