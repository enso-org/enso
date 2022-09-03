package org.enso.table.data.index;

import org.enso.table.data.column.storage.Storage;

public abstract class MultiValueKeyBase {
  protected final Storage[] storages;
  protected final int rowIndex;
  protected boolean hasFloatValues = false;
  protected boolean floatsComputed = false;

  public MultiValueKeyBase(Storage[] storage, int rowIndex) {
    this.storages = storage;
    this.rowIndex = rowIndex;
  }

  protected Object get(int column) {
    return storages[column].getItemBoxed(rowIndex);
  }

  @Override
  public abstract boolean equals(Object o);

  public boolean areAllNull() {
    for (Storage value : storages) {
      if (!value.isNa(rowIndex)) {
        return false;
      }
    }
    return true;
  }

  public boolean hasFloatValues() {
    if (!floatsComputed) {
      hasFloatValues = findFloats();
      floatsComputed = true;
    }

    return hasFloatValues;
  }

  private boolean findFloats() {
    for (int i = 0; i < storages.length; i++) {
      Object value = this.get(i);
      if (value != null) {
        Object folded = foldObject(value);
        if (folded instanceof Double) {
          return true;
        }
      }
    }
    return false;
  }

  protected static Object foldObject(Object value) {
    if (value instanceof Long) {
      return value;
    } else if (value instanceof Integer) {
      return ((Integer) value).longValue();
    } else if (value instanceof Byte) {
      return ((Byte) value).longValue();
    } else if (value instanceof Float && ((Float) value) % 1 == 0) {
      return ((Float) value).longValue();
    } else if (value instanceof Double && ((Double) value) % 1 == 0) {
      return ((Double) value).longValue();
    } else if (value instanceof Float) {
      return ((Float) value).doubleValue();
    } else if (value instanceof Double) {
      return value;
    }

    return value;
  }
}
