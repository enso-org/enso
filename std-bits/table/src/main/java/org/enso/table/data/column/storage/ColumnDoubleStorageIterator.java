package org.enso.table.data.column.storage;

import org.graalvm.polyglot.Context;

public interface ColumnDoubleStorageIterator extends ColumnStorageIterator<Double> {
  /** Gets the current item as a double. Note if the item isNothing value is undefined. */
  double getItemAsDouble();

  @FunctionalInterface
  interface DoubleDoubleZipper {
    void accept(long idx, double value1, boolean isNothing1, double value2, boolean isNothing2);
  }

  @FunctionalInterface
  interface DoubleLongZipper {
    void accept(long idx, double value1, boolean isNothing1, long value2, boolean isNothing2);
  }

  /** Zips this iterator with another iterator. */
  default void zip(ColumnDoubleStorage otherStorage, DoubleDoubleZipper zipper) {
    var other = otherStorage.iterator();
    Context context = Context.getCurrent();

    boolean hasValue1 = moveNext();
    boolean hasValue2 = other.moveNext();
    long idx = 0;
    while (hasValue1 || hasValue2) {
      boolean isNothing1 = !hasValue1 || isNothing();
      double value1 = isNothing1 ? Double.NaN : getItemAsDouble();
      boolean isNothing2 = !hasValue2 || other.isNothing();
      double value2 = isNothing2 ? Double.NaN : other.getItemAsDouble();
      zipper.accept(idx++, value1, isNothing1, value2, isNothing2);
      context.safepoint();
      hasValue1 = hasValue1 && moveNext();
      hasValue2 = hasValue2 && other.moveNext();
    }
  }

  /** Zips this iterator with another iterator. */
  default void zip(ColumnLongStorage otherStorage, DoubleLongZipper zipper) {
    var other = otherStorage.iterator();
    Context context = Context.getCurrent();

    boolean hasValue1 = moveNext();
    boolean hasValue2 = other.moveNext();
    long idx = 0;
    while (hasValue1 || hasValue2) {
      boolean isNothing1 = !hasValue1 || isNothing();
      double value1 = isNothing1 ? Double.NaN : getItemAsDouble();
      boolean isNothing2 = !hasValue2 || other.isNothing();
      long value2 = isNothing2 ? 0 : other.getItemAsLong();
      zipper.accept(idx++, value1, isNothing1, value2, isNothing2);
      context.safepoint();
      hasValue1 = hasValue1 && moveNext();
      hasValue2 = hasValue2 && other.moveNext();
    }
  }
}
