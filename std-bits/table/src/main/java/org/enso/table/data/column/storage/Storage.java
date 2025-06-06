package org.enso.table.data.column.storage;

import java.util.*;
import java.util.function.BiFunction;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.numeric.LongConstantStorage;
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

  /**
   * Returns a more specialized storage, if available.
   *
   * <p>This storage should have the same type as returned by {@code inferPreciseType(DEFAULT)}. See
   * {@link MixedStorage} for more information.
   */
  public Storage<?> tryGettingMoreSpecializedStorage() {
    return this;
  }

  @Override
  public abstract boolean isNothing(long index);

  @Override
  public abstract T getItemBoxed(long index);

  /**
   * @return the type of the values in this column's storage. Most storages just return their type.
   *     Mixed storage will try to see if all elements fit some more precise type.
   * @implNote The {@code PreciseTypeOptions.DEFAULT} should either be computable in constant time
   *     or cache its result for subsequent calls, as it may be called often.
   */
  public StorageType<?> inferPreciseType(PreciseTypeOptions options) {
    return getType();
  }

  /** A container for names of vectorizable operation. */
  public static final class Maps {
    public static final String MUL = "*";
    public static final String ADD = "+";
    public static final String SUB = "-";
    public static final String DIV = "/";
    public static final String MOD = "%";
    public static final String POWER = "^";
  }

  /**
   * Runs a vectorized operation on this storage, taking one scalar argument. Return null is not a
   * supported operation.
   */
  protected Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemAggregator problemAggregator) {
    return null;
  }

  /**
   * Runs a vectorized operation on this storage, taking a storage as the right argument -
   * processing row-by-row. Return null is not a supported operation.
   */
  protected Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemAggregator problemAggregator) {
    return null;
  }

  /**
   * Runs a 2-argument function on each element in this storage.
   *
   * @param function the function to run.
   * @param argument the argument to pass to each run of the function
   * @param skipNulls specifies whether null values on the input should result in a null result
   *     without passing them through the function, this is useful if the function does not support
   *     the null-values, but it needs to be set to false if the function should handle them.
   * @param expectedResultType the expected type for the result storage
   * @return a new storage containing results of the function for each row
   */
  public final Storage<?> binaryMap(
      BiFunction<Object, Object, Object> function,
      Object argument,
      boolean skipNulls,
      StorageType<?> expectedResultType,
      ProblemAggregator problemAggregator) {
    Builder storageBuilder = Builder.getForType(expectedResultType, getSize(), problemAggregator);
    if (skipNulls && argument == null) {
      // ToDo: appendNulls should take a long, not an int. Should have a constant Storage for null.
      storageBuilder.appendNulls((int) getSize());
      return storageBuilder.seal();
    }

    Context context = Context.getCurrent();
    for (long i = 0; i < getSize(); i++) {
      Object it = getItemBoxed(i);
      if (skipNulls && it == null) {
        storageBuilder.appendNulls(1);
      } else {
        Object result = function.apply(it, argument);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        storageBuilder.append(converted);
      }

      context.safepoint();
    }
    return storageBuilder.seal();
  }

  /**
   * Runs a function on each pair of non-missing elements in this and arg.
   *
   * @param function the function to run.
   * @param skipNa whether rows containing missing values should be passed to the function.
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage<?> zip(
      BiFunction<Object, Object, Object> function,
      Storage<?> arg,
      boolean skipNa,
      StorageType<?> expectedResultType,
      ProblemAggregator problemAggregator) {
    Builder storageBuilder = Builder.getForType(expectedResultType, getSize(), problemAggregator);
    Context context = Context.getCurrent();
    for (long i = 0; i < getSize(); i++) {
      Object it1 = getItemBoxed(i);
      Object it2 = i < arg.getSize() ? arg.getItemBoxed(i) : null;
      if (skipNa && (it1 == null || it2 == null)) {
        storageBuilder.appendNulls(1);
      } else {
        Object result = function.apply(it1, it2);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        storageBuilder.append(converted);
      }

      context.safepoint();
    }
    return storageBuilder.seal();
  }

  /**
   * Runs a binary operation with a scalar argument.
   *
   * <p>If a vectorized implementation is available, it is used, otherwise the fallback is used.
   *
   * @param name the name of the vectorized operation
   * @param problemAggregator the problem aggregator to use for the vectorized implementation
   * @param fallback the fallback Enso function to run if vectorized implementation is not
   *     available; it should never raise dataflow errors.
   * @param argument the argument to pass to each run of the function
   * @param skipNulls specifies whether null values on the input should result in a null result
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @return the result of running the operation on each row
   */
  public final Storage<?> vectorizedOrFallbackBinaryMap(
      String name,
      MapOperationProblemAggregator problemAggregator,
      BiFunction<Object, Object, Object> fallback,
      Object argument,
      boolean skipNulls,
      StorageType<?> expectedResultType) {
    var binaryMap = runVectorizedBinaryMap(name, argument, problemAggregator);
    if (binaryMap != null) {
      return binaryMap;
    }

    checkFallback(fallback, expectedResultType, name);
    return binaryMap(fallback, argument, skipNulls, expectedResultType, problemAggregator);
  }

  /**
   * Runs a binary operation with a storage argument.
   *
   * <p>If a vectorized implementation is available, it is used, otherwise the fallback is used.
   *
   * @param name the name of the vectorized operation
   * @param problemAggregator the problem aggregator to use for the vectorized implementation
   * @param fallback the fallback Enso function to run if vectorized implementation is not
   *     available; it should never raise dataflow errors.
   * @param other the other storage to zip with this one
   * @param skipNulls specifies whether null values on the input should result in a null result
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @return the result of running the operation on each row
   */
  public final Storage<?> vectorizedOrFallbackZip(
      String name,
      MapOperationProblemAggregator problemAggregator,
      BiFunction<Object, Object, Object> fallback,
      Storage<?> other,
      boolean skipNulls,
      StorageType<?> expectedResultType) {
    var binaryZip = runVectorizedZip(name, other, problemAggregator);
    if (binaryZip != null) {
      return binaryZip;
    }

    checkFallback(fallback, expectedResultType, name);
    return zip(fallback, other, skipNulls, expectedResultType, problemAggregator);
  }

  private void checkFallback(Object fallback, StorageType<?> storageType, String operationName)
      throws IllegalArgumentException {
    if (fallback == null) {
      if (operationName == null) {
        throw new IllegalArgumentException(
            "A function or name of vectorized operation must be specified. This is a bug in the"
                + " Table library.");
      } else {
        String className = this.getClass().getName();
        throw new IllegalArgumentException(
            "The operation "
                + operationName
                + " has no vectorized implementation for "
                + className
                + ", but no fallback function was provided. This is a bug in the Table library.");
      }
    }

    if (storageType == null) {
      throw new IllegalArgumentException(
          "The expected result type must be specified if a fallback function is used. This is a bug"
              + " in the Table library.");
    }
  }

  /**
   * Return a new storage, where missing elements have been replaced by arg.
   *
   * @param arg the value to use for missing elements
   * @param commonType the common type of this storage and the provided value
   * @return a new storage, with all missing elements replaced by arg
   */
  public Storage<?> fillMissing(
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
  public Storage<?> fillMissingFrom(
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
  public abstract Storage<?> fillMissingFromPrevious(BoolStorage missingIndicator);

  /**
   * Return a new storage, containing only the items marked true in the mask.
   *
   * @param filterMask the mask to use
   * @param newLength the number of true values in mask
   * @return a new storage, filtered with the given mask
   */
  public abstract Storage<T> applyFilter(BitSet filterMask, int newLength);

  /**
   * Returns a new storage, ordered according to the rules specified in a mask.
   *
   * @param mask@return a storage resulting from applying the reordering rules
   */
  public abstract Storage<T> applyMask(OrderMask mask);

  /**
   * @return a copy of the storage containing a slice of the original data
   */
  public abstract Storage<T> slice(int offset, int limit);

  /**
   * @return a new storage instance, containing the same elements as this one, with {@code count}
   *     nulls appended at the end
   */
  public abstract Storage<?> appendNulls(int count);

  /**
   * @return a copy of the storage consisting of slices of the original data
   */
  public abstract Storage<T> slice(List<SliceRange> ranges);

  /**
   * Counts the number of times each value has been seen before in this storage.
   *
   * @return a storage counting the number of times each value in this one has been seen before.
   */
  public Storage<?> duplicateCount() {
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

  /** Creates a storage containing a single repeated item. */
  public static Storage<?> fromRepeatedItem(
      Value item, int repeat, ProblemAggregator problemAggregator) {
    if (repeat < 0) {
      throw new IllegalArgumentException("Repeat count must be non-negative.");
    }

    Object converted = Polyglot_Utils.convertPolyglotValue(item);

    if (converted == null) {
      return new NullStorage(repeat);
    }

    if (converted instanceof Long longValue) {
      return new LongConstantStorage(longValue, repeat);
    }

    var storageType = StorageType.forBoxedItem(converted, PreciseTypeOptions.DEFAULT);
    Builder builder = Builder.getForType(storageType, repeat, problemAggregator);
    Context context = Context.getCurrent();
    for (int i = 0; i < repeat; i++) {
      builder.append(converted);
      context.safepoint();
    }

    return builder.seal();
  }

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
}
