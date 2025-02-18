package org.enso.table.data.column.storage;

import org.graalvm.polyglot.Context;

public interface ColumnLongStorageIterator extends ColumnStorageIterator<Long> {
  /** Gets the current item as a long. Note if the item isNothing value is undefined. */
  long getItemAsLong();

  @FunctionalInterface
  interface LongLongZipper {
    void accept(long idx, long value1, boolean isNothing1, long value2, boolean isNothing2);
  }

  @FunctionalInterface
  interface LongDoubleZipper {
    void accept(long idx, long value1, boolean isNothing1, double value2, boolean isNothing2);
  }

  /** Zips this iterator with a Double storage. */
  default void zip(ColumnDoubleStorage otherStorage, LongDoubleZipper zipper) {
    var other = otherStorage.iterator();
    Context context = Context.getCurrent();

    boolean hasValue1 = moveNext();
    boolean hasValue2 = other.moveNext();
    long idx = 0;
    while (hasValue1 || hasValue2) {
      boolean isNothing1 = !hasValue1 || isNothing();
      long value1 = isNothing1 ? 0 : getItemAsLong();
      boolean isNothing2 = !hasValue2 || other.isNothing();
      double value2 = isNothing2 ? 0 : other.getItemAsDouble();
      zipper.accept(idx++, value1, isNothing1, value2, isNothing2);
      context.safepoint();
      hasValue1 = hasValue1 && moveNext();
      hasValue2 = hasValue2 && other.moveNext();
    }
  }

  /** Zips this iterator with another Long storage. */
  default void zip(ColumnLongStorage otherStorage, LongLongZipper zipper) {
    var other = otherStorage.iterator();
    Context context = Context.getCurrent();

    boolean hasValue1 = moveNext();
    boolean hasValue2 = other.moveNext();
    long idx = 0;
    while (hasValue1 || hasValue2) {
      boolean isNothing1 = !hasValue1 || isNothing();
      long value1 = isNothing1 ? 0 : getItemAsLong();
      boolean isNothing2 = !hasValue2 || other.isNothing();
      long value2 = isNothing2 ? 0 : other.getItemAsLong();
      zipper.accept(idx++, value1, isNothing1, value2, isNothing2);
      context.safepoint();
      hasValue1 = hasValue1 && moveNext();
      hasValue2 = hasValue2 && other.moveNext();
    }
  }
}
