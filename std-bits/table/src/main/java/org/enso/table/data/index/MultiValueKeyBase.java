package org.enso.table.data.index;

import java.util.ArrayList;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;

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
  public Object get(int column) {
    return storages[column].getItemBoxed(rowIndex);
  }

  public List<Object> getValues() {
    List<Object> result = new ArrayList<>(storages.length);
    for (int i = 0; i < storages.length; i++) {
      result.add(get(i));
    }
    return result;
  }

  @Override
  public abstract boolean equals(Object o);

  /** Checks if all cells in the current row are missing. */
  public boolean areAllNull() {
    for (Storage<?> storage : storages) {
      if (!storage.isNa(rowIndex)) {
        return false;
      }
    }
    return true;
  }

  /** Checks if any cells in the current row are missing. */
  public boolean hasAnyNulls() {
    for (Storage<?> storage : storages) {
      if (storage.isNa(rowIndex)) {
        return true;
      }
    }
    return false;
  }

  /* Checks if any cell contains float values.

   It takes value folding into account, i.e. a float value that can be coerced to an integer without loss of
   precision is not considered floating.
  */
  public boolean hasFloatValues() {
    if (!floatsComputed) {
      hasFloatValues = findFloats();
      floatsComputed = true;
    }

    return hasFloatValues;
  }

  public interface ColumnNameMapping {
    String getColumnName(int columnIx);
  }

  public void checkAndReportFloatingEquality(
      ColumnAggregatedProblemAggregator problemAggregator, ColumnNameMapping columnNameMapping) {
    if (hasFloatValues()) {
      for (int columnIx = 0; columnIx < storages.length; columnIx++) {
        Object value = this.get(columnIx);
        if (NumericConverter.isFloatLike(value)) {
          problemAggregator.reportColumnAggregatedProblem(
              new FloatingPointGrouping(columnNameMapping.getColumnName(columnIx), rowIndex));
        }
      }
    }
  }

  private boolean findFloats() {
    for (int i = 0; i < storages.length; i++) {
      Object value = this.get(i);
      if (NumericConverter.isFloatLike(value)) {
        return true;
      }
    }
    return false;
  }
}
