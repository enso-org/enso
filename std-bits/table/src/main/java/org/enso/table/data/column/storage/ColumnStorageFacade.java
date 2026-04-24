package org.enso.table.data.column.storage;

import java.util.Iterator;
import java.util.function.Function;
import org.enso.table.data.column.storage.type.StorageType;

/** A facade for a column storage that converts the stored type to another type. */
public final class ColumnStorageFacade<S, T> implements ColumnStorage<T> {
  private final char typeChar;
  private final long typeSize;
  private final ColumnStorage<S> parent;
  private final Function<S, T> converter;

  public ColumnStorageFacade(
      StorageType<T> storageType, ColumnStorage<S> parent, Function<S, T> converter) {
    this.typeChar = storageType.typeChar();
    this.typeSize = storageType.size();
    this.parent = parent;
    this.converter = converter;
  }

  @Override
  public long uniqueKey() {
    return parent.uniqueKey();
  }

  @Override
  public long getSize() {
    return parent.getSize();
  }

  @Override
  public char typeChar() {
    return typeChar;
  }

  @Override
  public long typeSize() {
    return typeSize;
  }

  @Override
  public boolean isNothing(long index) {
    return parent.isNothing(index);
  }

  @Override
  public T getItemBoxed(long index) {
    S item = parent.getItemBoxed(index);
    return item == null ? null : converter.apply(item);
  }

  @Override
  public Iterator<T> iterator() {
    return new Iterator<T>() {
      private final Iterator<S> parentIterator = parent.iterator();

      @Override
      public boolean hasNext() {
        return parentIterator.hasNext();
      }

      @Override
      public T next() {
        S item = parentIterator.next();
        if (item == null) {
          return null;
        } else {
          return converter.apply(item);
        }
      }
    };
  }
}
