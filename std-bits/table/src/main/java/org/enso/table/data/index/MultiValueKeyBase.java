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

  protected Object foldObject(Object value) {
    if (value instanceof Long) {
      return value;
    } else if (value instanceof Integer i) {
      return i.longValue();
    } else if (value instanceof Byte b) {
      return b.longValue();
    } else if (value instanceof Float f && f % 1 == 0) {
      return f.longValue();
    } else if (value instanceof Double d && d % 1 == 0) {
      return d.longValue();
    } else if (value instanceof Float f) {
      return f.doubleValue();
    } else if (value instanceof Double d) {
      return d;
    }

    return value;
  }
}
