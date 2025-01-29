package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.NullType;

/** A builder for boxed object columns. */
public class ObjectBuilder extends TypedBuilder<Object> {
  public ObjectBuilder(int size) {
    super(AnyObjectType.INSTANCE, new Object[size]);
  }

  @Override
  public boolean accepts(Object o) {
    return true;
  }

  @Override
  public void append(Object o) {
    ensureSpaceToAppend();
    data[currentSize++] = o;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    long newSize = currentSize + storage.getSize();
    if (newSize > data.length) {
      int newSizeInt = Builder.checkSize(newSize);
      resize(newSizeInt);
    }

    if (storage instanceof SpecializedStorage<?> specializedStorage) {
      // We can safely cast here, as for SpecializedStorage the size is always an int.
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
  public Storage<Object> doSeal() {
    return new ObjectStorage(data);
  }
}
