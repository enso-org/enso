package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.NullType;

/** A builder for boxed object columns. */
class ObjectBuilder extends TypedBuilder<Object> {
  ObjectBuilder(int size) {
    super(AnyObjectType.INSTANCE, new Object[size]);
  }

  @Override
  public boolean accepts(Object o) {
    return true;
  }

  @Override
  public ObjectBuilder append(Object o) {
    ensureSpaceToAppend();
    data[currentSize++] = o;
    return this;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    long newSize = currentSize + storage.getSize();
    if (newSize > data.length) {
      int newSizeInt = Builder.checkSize(newSize);
      resize(newSizeInt);
    }

    if (storage instanceof TypedStorage<?> specializedStorage) {
      // We can safely cast here, as for TypedStorage the size is always an int.
      int toCopy = (int) storage.getSize();
      System.arraycopy(specializedStorage.getData(), 0, data, currentSize, toCopy);
      currentSize += toCopy;
    } else if (storage.getType() instanceof NullType) {
      appendNulls(Math.toIntExact(storage.getSize()));
    } else {
      long n = storage.getSize();
      for (long i = 0; i < n; i++) {
        append(storage.getItemBoxed(i));
      }
    }
  }

  @Override
  public ColumnStorage<Object> doSeal() {
    return new TypedStorage<>(AnyObjectType.INSTANCE, data);
  }
}
