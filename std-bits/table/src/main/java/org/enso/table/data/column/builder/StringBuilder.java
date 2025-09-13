package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for string columns. */
final class StringBuilder extends TypedBuilder<String> {
  private final TextType type;

  StringBuilder(int size, TextType type) {
    super(type, new String[size]);
    this.type = type;
  }

  @Override
  public StringBuilder append(Object o) {
    ensureSpaceToAppend();
    if (o == null) {
      appendNulls(1);
    } else {
      try {
        String str = (String) o;
        if (type.fits(str)) {
          data[currentSize++] = str;
        } else {
          throw new ValueTypeMismatchException(type, str);
        }
      } catch (ClassCastException e) {
        throw new ValueTypeMismatchException(type, o);
      }
    }
    return this;
  }

  @Override
  public boolean accepts(Object o) {
    if (o instanceof String s) {
      return type.fits(s);
    } else {
      return false;
    }
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof TextType gotType
        && type.fitsExactly(gotType)
        && storage instanceof TypedStorage<?>) {
      // This cast is safe, because storage.getType() == this.getType() == TextType iff
      // storage.T == String
      @SuppressWarnings("unchecked")
      TypedStorage<String> specializedStorage = (TypedStorage<String>) storage;
      int toCopy = (int) storage.getSize();
      System.arraycopy(specializedStorage.getData(), 0, data, currentSize, toCopy);
      currentSize += toCopy;
      return;
    }

    super.appendBulkStorage(storage);
  }

  @Override
  protected ColumnStorage<String> doSeal() {
    return new TypedStorage<>(type, data);
  }
}
