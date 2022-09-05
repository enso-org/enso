package org.enso.table.data.index;

import org.enso.table.data.column.storage.Storage;

/** The base class for keys used for sorting/grouping rows by a set of columns. */
public abstract class MultiValueKeyBase {
  protected final Storage[] storages;
  protected final int rowIndex;
  protected boolean hasFloatValues = false;
  protected boolean floatsComputed = false;

  /**
   * Constructs a key based on an array of column storages and the index of the row the key is
   * associated with.
   */
  public MultiValueKeyBase(Storage[] storage, int rowIndex) {
    this.storages = storage;
    this.rowIndex = rowIndex;
  }

  /** A helper function to get the item from the nth column of the key's row. */
  protected Object get(int column) {
    return storages[column].getItemBoxed(rowIndex);
  }

  @Override
  public abstract boolean equals(Object o);

  /** Checks if all cells in the current row are missing. */
  public boolean areAllNull() {
    for (Storage value : storages) {
      if (!value.isNa(rowIndex)) {
        return false;
      }
    }
    return true;
  }

  /* Checks if any cell contains float values.

   It takes value folding into account, i.e. a float value that can be coerced to an integer without loss of precision is not considered floating.
  */
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
        Object folded = foldNumeric(value);
        if (folded instanceof Double) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * If the value is a numeric type, this method coerces it in such a way to ensure consistency with
   * Enso.
   *
   * <p>Integer types are coerced to {@code Long} and floating point values are coerced to {@code
   * Double} unless they represent a whole integer in which case they are also coerced to {@code
   * Long}, to ensure the Enso property that {@code 2 == 2.0}.
   *
   * Returns {@code null} if the value was not a numeric value.
   */
  protected Object foldNumeric(Object value) {
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

    return null;
  }
}
