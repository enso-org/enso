package org.enso.table.data.column.storage;

/** A marker interface allowing to refine the type of the storage's stored elements. */
public interface TypedStorage<T> {
  T getItemBoxed(int idx);
}
