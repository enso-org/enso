package org.enso.table.data.column.storage.numeric;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.iterators.LongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.graalvm.polyglot.Context;

public abstract class AbstractLongStorage extends Storage<Long> implements ColumnLongStorage {
  private final long size;
  private final IntegerType type;

  protected AbstractLongStorage(long size, IntegerType type) {
    this.size = size;
    this.type = type;
  }

  @Override
  public final long getSize() {
    return size;
  }

  @Override
  public IntegerType getType() {
    return type;
  }

  @Override
  public Long getItemBoxed(long index) {
    return isNothing(index) ? null : getItemAsLong(index);
  }

  @Override
  public abstract boolean isNothing(long idx);

  @Override
  public abstract long getItemAsLong(long index) throws ValueIsNothingException;

  /**
   * Return an instance of storage containing the same data but with a wider type.
   *
   * <p>Ideally it should avoid copying the data, if it's possible.
   */
  public abstract AbstractLongStorage widen(IntegerType widerType);

  @Override
  public ColumnStorage<Long> applyMask(OrderMask mask) {
    var builder = Builder.getForLong(getType(), mask.length(), BlackholeProblemAggregator.INSTANCE);
    Context context = Context.getCurrent();
    for (int i = 0; i < mask.length(); i++) {
      int position = mask.get(i);
      if (position == OrderMask.NOT_FOUND_INDEX || isNothing(position)) {
        builder.appendNulls(1);
      } else {
        builder.appendLong(getItemAsLong(position));
      }

      context.safepoint();
    }
    return builder.seal();
  }

  @Override
  public ColumnLongStorageIterator iteratorWithIndex() {
    return new LongStorageIterator(this);
  }
}
