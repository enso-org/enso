package org.enso.table.data.index;

import java.util.ArrayList;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.graalvm.polyglot.Context;

/** The base class for keys used for sorting/grouping rows by a set of columns. */
public abstract class MultiValueKeyBase {
  protected final Storage<?>[] storages;
  protected final int rowIndex;
  protected boolean hasFloatValues = false;
  protected boolean floatsComputed = false;

  /**
   * Constructs a key based on an array of column storages and the index of the row the key is
   * associated with.
   */
  public MultiValueKeyBase(Storage<?>[] storage, int rowIndex) {
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
    Context context = Context.getCurrent();
    for (Storage<?> storage : storages) {
      if (!storage.isNa(rowIndex)) {
        return false;
      }

      context.safepoint();
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
    Context context = Context.getCurrent();
    for (int i = 0; i < storages.length; i++) {
      Object value = this.get(i);
      if (NumericConverter.isDecimalLike(value)) {
        return true;
      }

      context.safepoint();
    }
    return false;
  }

  /**
   * Finds which columns contain a float value at this index position and returns their positions in
   * this index.
   */
  public List<Integer> floatColumnPositions() {
    Context context = Context.getCurrent();
    List<Integer> result = new ArrayList<>();
    for (int i = 0; i < storages.length; i++) {
      Object value = this.get(i);
      if (NumericConverter.isDecimalLike(value)) {
        result.add(i);
      }

      context.safepoint();
    }
    return result;
  }
}
