package org.enso.table.data.column.storage;

public interface ColumnBooleanStorageIterator extends ColumnStorageIterator<Boolean> {
  /** Gets the current item as a boolean. Note if the item isNothing value is undefined. */
  boolean getItemAsBoolean();

  @FunctionalInterface
  interface BooleanBooleanZipper {
    void accept(long idx, boolean value1, boolean isNothing1, boolean value2, boolean isNothing2);
  }

  /** Zips this iterator with another iterator. */
  default void zip(
      ColumnBooleanStorage otherStorage, ColumnBooleanStorageIterator.BooleanBooleanZipper zipper) {
    var other = otherStorage.iterator();

    boolean hasValue1 = moveNext();
    boolean hasValue2 = other.moveNext();
    long idx = 0;
    while (hasValue1 || hasValue2) {
      boolean isNothing1 = !hasValue1 || isNothing();
      boolean value1 = isNothing1 && getItemAsBoolean();
      boolean isNothing2 = !hasValue2 || other.isNothing();
      boolean value2 = isNothing2 && other.getItemAsBoolean();
      zipper.accept(idx++, value1, isNothing1, value2, isNothing2);
      hasValue1 = hasValue1 && moveNext();
      hasValue2 = hasValue2 && other.moveNext();
    }
  }
}
