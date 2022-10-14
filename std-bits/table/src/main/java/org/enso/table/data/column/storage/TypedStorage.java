package org.enso.table.data.column.storage;

public interface TypedStorage<T> {
  int size();
  boolean isNa(long idx);
  T getItemTyped(int idx);
}
