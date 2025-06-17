package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** An abstract representation of a data column. */
public abstract class Storage<T> implements ColumnStorage<T> {
  @Override
  public abstract long getSize();

  @Override
  public abstract StorageType<T> getType();

  @Override
  public abstract boolean isNothing(long index);

  @Override
  public abstract T getItemBoxed(long index);

  @Override
  public Iterator<T> iterator() {
    return new Iterator<>() {
      private long index = -1;

      @Override
      public boolean hasNext() {
        return index + 1 < getSize();
      }

      @Override
      public T next() {
        if (!hasNext()) {
          throw new NoSuchElementException();
        }
        return getItemBoxed(++index);
      }
    };
  }

  @Override
  public ColumnStorageIterator<T> iteratorWithIndex() {
    return new StorageIterator<>(this);
  }

  public static class StorageIterator<T> implements ColumnStorageIterator<T> {
    protected final ColumnStorage<T> parent;
    protected long index = -1;

    public StorageIterator(ColumnStorage<T> parent) {
      this.parent = parent;
    }

    @Override
    public T getItemBoxed() {
      return parent.getItemBoxed(index);
    }

    @Override
    public boolean isNothing() {
      return parent.isNothing(index);
    }

    @Override
    public boolean hasNext() {
      return index + 1 < parent.getSize();
    }

    @Override
    public T next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }
      return parent.getItemBoxed(++index);
    }

    @Override
    public long getIndex() {
      return index;
    }

    @Override
    public boolean moveNext() {
      if (!hasNext()) {
        return false;
      }
      index++;
      return true;
    }
  }

  /**
   * @return the type of the values in this column's storage. Most storages just return their type.
   *     Mixed storage will try to see if all elements fit some more precise type.
   * @implNote The {@code PreciseTypeOptions.DEFAULT} should either be computable in constant time
   *     or cache its result for subsequent calls, as it may be called often.
   */
  public StorageType<?> inferPreciseType(PreciseTypeOptions options) {
    return getType();
  }

  /**
   * Return a new storage, where missing elements have been replaced by arg.
   *
   * @param arg the value to use for missing elements
   * @param commonType the common type of this storage and the provided value
   * @return a new storage, with all missing elements replaced by arg
   */
  public ColumnStorage<?> fillMissing(
      Value arg, StorageType<?> commonType, ProblemAggregator problemAggregator) {
    Builder builder = Builder.getForType(commonType, getSize(), problemAggregator);
    Object convertedFallback = Polyglot_Utils.convertPolyglotValue(arg);
    Context context = Context.getCurrent();
    for (long i = 0; i < getSize(); i++) {
      Object it = getItemBoxed(i);
      builder.append(it == null ? convertedFallback : it);
      context.safepoint();
    }

    return builder.seal();
  }

  /**
   * Fills missing values in this storage, by using corresponding values from {@code other}.
   *
   * @param other the source of default values
   * @param commonType a common type that should fit values from both storages
   * @return a new storage with missing values filled
   */
  public ColumnStorage<?> fillMissingFrom(
      Storage<?> other, StorageType<?> commonType, ProblemAggregator problemAggregator) {
    var builder = Builder.getForType(commonType, getSize(), problemAggregator);
    Context context = Context.getCurrent();
    for (long i = 0; i < getSize(); i++) {
      builder.append(isNothing(i) ? other.getItemBoxed(i) : getItemBoxed(i));
      context.safepoint();
    }
    return builder.seal();
  }

  /**
   * Fills missing values with a previous non-missing value.
   *
   * <p>
   *
   * @param missingIndicator Specifies which values should be considered missing. It can be used to
   *     implement custom missing value semantics, like `fill_empty`. It can be set to {@code null}
   *     to just rely on the default semantics of missing values. Some storages may not allow
   *     customizing the semantics.
   */
  public abstract ColumnStorage<?> fillMissingFromPrevious(BoolStorage missingIndicator);

  /**
   * Return a new storage, containing only the items marked true in the mask.
   *
   * @param filterMask the mask to use
   * @param newLength the number of true values in mask
   * @return a new storage, filtered with the given mask
   */
  public abstract ColumnStorage<T> applyFilter(BitSet filterMask, int newLength);

  /**
   * Returns a new storage, ordered according to the rules specified in a mask.
   *
   * @param mask@return a storage resulting from applying the reordering rules
   */
  public abstract ColumnStorage<T> applyMask(OrderMask mask);

  /**
   * @return a copy of the storage containing a slice of the original data
   */
  public abstract ColumnStorage<T> slice(int offset, int limit);

  /**
   * @return a copy of the storage consisting of slices of the original data
   */
  public abstract ColumnStorage<T> slice(List<SliceRange> ranges);

  /**
   * Counts the number of times each value has been seen before in this storage.
   *
   * @return a storage counting the number of times each value in this one has been seen before.
   */
  public ColumnStorage<?> duplicateCount() {
    HashMap<Object, Integer> occurenceCount = new HashMap<>();
    Context context = Context.getCurrent();
    var builder =
        Builder.getForLong(IntegerType.INT_64, getSize(), BlackholeProblemAggregator.INSTANCE);
    for (long i = 0; i < getSize(); i++) {
      var value = getItemBoxed(i);
      var count = occurenceCount.getOrDefault(value, 0);
      builder.appendLong(count);
      occurenceCount.put(value, count + 1);
      context.safepoint();
    }
    return builder.seal();
  }
}
