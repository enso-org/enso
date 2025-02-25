package org.enso.table.data.column.operation;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;

public interface BinaryOperation {
  static ColumnStorage<?> getInferredStorage(Column input) {
    var storage = input.getStorage();
    if (storage instanceof ColumnStorageWithInferredStorage withInferredStorage) {
      var inferredStorage = withInferredStorage.getInferredStorage();
      if (inferredStorage != null) {
        return inferredStorage;
      }
    }
    return storage;
  }

  /*
   * Gets the storage of the column resolving through inferred storages.
   * Replace with a simple call to `getStorage` if an operation should not
   * resolve inferred storages.
   * */
  default ColumnStorage<?> getStorage(Column input) {
    return getInferredStorage(input);
  }

  /**
   * Checks if the operation can be applied to the given columns.
   *
   * @param left the left column.
   * @param right the right column or value.
   * @return true if the operation can be applied, false otherwise.
   */
  default boolean canApply(Column left, Object right) {
    return right instanceof Column rightColumn
        ? canApplyZip(getStorage(left), getStorage(rightColumn))
        : canApplyMap(getStorage(left), right);
  }

  /**
   * Applies the operation to the given columns.
   *
   * @param left the left column.
   * @param right the right column or value.
   * @param newName the name of the new column.
   * @return the result of the operation.
   */
  default Column apply(Column left, Object right, String newName) {
    ColumnStorage<?> leftStorage = getStorage(left);

    ColumnStorage<?> output;
    if (right instanceof Column rightColumn) {
      ColumnStorage<?> rightStorage = getStorage(rightColumn);
      if (!canApplyZip(leftStorage, rightStorage)) {
        throw new IllegalArgumentException("Cannot apply zip");
      }
      output = applyZip(leftStorage, rightStorage);
    } else {
      if (!canApplyMap(leftStorage, right)) {
        throw new IllegalArgumentException("Cannot apply map");
      }
      output = applyMap(leftStorage, right);
    }

    return new Column(newName, (Storage<?>) output);
  }

  /** Can the map be applied to the pair of ColumnStorage and constant? */
  boolean canApplyMap(ColumnStorage<?> left, Object rightValue);

  /** Can the map be applied to the pair of ColumnStorage? */
  boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right);

  /** Apply the map to the pair of ColumnStorage and constant. */
  ColumnStorage<Boolean> applyMap(ColumnStorage<?> left, Object rightValue);

  /** Apply the map to the pair of ColumnStorage. */
  ColumnStorage<Boolean> applyZip(ColumnStorage<?> left, ColumnStorage<?> right);
}
