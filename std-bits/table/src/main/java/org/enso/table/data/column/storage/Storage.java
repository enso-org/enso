package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.builder.object.ObjectBuilder;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.graalvm.polyglot.Value;

/** An abstract representation of a data column. */
public abstract class Storage<T> {
  /** @return the number of elements in this column (including NAs) */
  public abstract int size();

  /** @return the number of NA elements in this column */
  public abstract int countMissing();

  /** @return the type tag of this column's storage. Must be one of {@link Type} */
  public abstract int getType();

  /**
   * Checks whether the value at {@code idx} is missing.
   *
   * @param idx the index to check.
   * @return whether or not the value is missing.
   */
  public abstract boolean isNa(long idx);

  /**
   * Returns a boxed representation of an item. Missing values are denoted with null.
   *
   * @param idx the index to look up
   * @return the item at position {@code idx}
   */
  public abstract T getItemBoxed(int idx);

  /**
   * Enumerating possible storage types.
   *
   * <p>Keep in sync with variables in {@code Standard.Table.Data.Column}. These variables are
   * copied between Enso and Java code, in order to make them trivially constant on the Enso side,
   * without invoking the polyglot machinery to access them.
   */
  public static final class Type {
    public static final int OBJECT = 0;
    public static final int LONG = 1;
    public static final int DOUBLE = 2;
    public static final int STRING = 3;
    public static final int BOOL = 4;
    public static final int DATE = 5;
    public static final int TIME_OF_DAY = 6;
    public static final int DATE_TIME = 7;
  }

  /** A container for names of vectorizable operation. */
  public static final class Maps {
    public static final String EQ = "==";
    public static final String LT = "<";
    public static final String LTE = "<=";
    public static final String GT = ">";
    public static final String GTE = ">=";
    public static final String MUL = "*";
    public static final String ADD = "+";
    public static final String SUB = "-";
    public static final String DIV = "/";
    public static final String MOD = "%";
    public static final String POWER = "^";
    public static final String NOT = "not";
    public static final String AND = "&&";
    public static final String OR = "||";
    public static final String IS_MISSING = "is_missing";
    public static final String IS_NAN = "is_nan";
    public static final String IS_EMPTY = "is_empty";
    public static final String STARTS_WITH = "starts_with";
    public static final String ENDS_WITH = "ends_with";
    public static final String CONTAINS = "contains";
    public static final String LIKE = "like";
    public static final String IS_IN = "is_in";
  }

  /**
   * Specifies if the given operation has a vectorized implementation available for this storage.
   */
  public abstract boolean isOpVectorized(String name);

  protected abstract Storage<?> runVectorizedMap(
      String name, Object argument, MapOperationProblemBuilder problemBuilder);

  protected abstract Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder);

  /**
   * Runs a function on each non-missing element in this storage and gathers the results.
   *
   * @param name a name of potential vectorized variant of the function that should be used if
   *     supported. If this argument is null, the vectorized operation will never be used.
   * @param function the function to run.
   * @param argument the argument to pass to each run of the function
   * @param skipNulls specifies whether null values on the input should result in a null result
   *     without passing them through the function, this is useful if the function does not support
   *     the null-values, but it needs to be set to false if the function should handle them.
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage<?> bimap(
      String name,
      BiFunction<Object, Object, Object> function,
      Object argument,
      boolean skipNulls,
      MapOperationProblemBuilder problemBuilder) {
    if (name != null && isOpVectorized(name)) {
      return runVectorizedMap(name, argument, problemBuilder);
    }
    Builder builder = new InferredBuilder(size());
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (skipNulls && it == null) {
        builder.appendNoGrow(null);
      } else {
        Object result = function.apply(it, argument);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        builder.appendNoGrow(converted);
      }
    }
    return builder.seal();
  }

  /**
   * Runs a function on each non-missing element in this storage and gathers the results.
   *
   * @param name a name of potential vectorized variant of the function that should be used if
   *     supported. If this argument is null, the vectorized operation will never be used.
   * @param function the function to run.
   * @param onMissing the value to place for missing cells, usually just null
   * @param problemBuilder a builder for reporting computation problems
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage<?> map(
      String name,
      Function<Object, Value> function,
      Value onMissing,
      MapOperationProblemBuilder problemBuilder) {
    if (name != null && isOpVectorized(name)) {
      return runVectorizedMap(name, null, problemBuilder);
    }
    Object missingValue = Polyglot_Utils.convertPolyglotValue(onMissing);
    Builder builder = new InferredBuilder(size());
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (it == null) {
        builder.appendNoGrow(missingValue);
      } else {
        Value result = function.apply(it);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        builder.appendNoGrow(converted);
      }
    }
    return builder.seal();
  }

  /**
   * Runs a function on each pair of non-missing elements in this and arg.
   *
   * @param name a name of potential vectorized variant of the function that should be used if
   *     supported. If this argument is null, the vectorized operation will never be used.
   * @param function the function to run.
   * @param skipNa whether rows containing missing values should be passed to the function.
   * @param problemBuilder the builder used for reporting computation problems
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage<?> zip(
      String name,
      BiFunction<Object, Object, Object> function,
      Storage<?> arg,
      boolean skipNa,
      MapOperationProblemBuilder problemBuilder) {
    if (name != null && isOpVectorized(name)) {
      return runVectorizedZip(name, arg, problemBuilder);
    }
    Builder builder = new InferredBuilder(size());
    for (int i = 0; i < size(); i++) {
      Object it1 = getItemBoxed(i);
      Object it2 = i < arg.size() ? arg.getItemBoxed(i) : null;
      if (skipNa && (it1 == null || it2 == null)) {
        builder.appendNoGrow(null);
      } else {
        Object result = function.apply(it1, it2);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        builder.appendNoGrow(converted);
      }
    }
    return builder.seal();
  }

  /**
   * Return a new storage, where missing elements have been replaced by arg.
   *
   * @param arg the value to use for missing elements
   * @return a new storage, with all missing elements replaced by arg
   */
  public Storage<?> fillMissing(Value arg) {
    return fillMissingHelper(arg, new ObjectBuilder(size()));
  }

  /**
   * Fills missing values in this storage, by using corresponding values from {@code other}.
   *
   * @param other the source of default values
   * @return a new storage with missing values filled
   */
  public Storage<?> fillMissingFrom(Storage<?> other) {
    var builder = new InferredBuilder(size());
    for (int i = 0; i < size(); i++) {
      if (isNa(i)) {
        builder.appendNoGrow(other.getItemBoxed(i));
      } else {
        builder.appendNoGrow(getItemBoxed(i));
      }
    }
    return builder.seal();
  }

  protected final Storage<?> fillMissingHelper(Value arg, Builder builder) {
    Object convertedFallback = Polyglot_Utils.convertPolyglotValue(arg);
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (it == null) {
        builder.appendNoGrow(convertedFallback);
      } else {
        builder.appendNoGrow(it);
      }
    }
    return builder.seal();
  }

  /**
   * Return a new storage, containing only the items marked true in the mask.
   *
   * @param mask the mask to use
   * @param cardinality the number of true values in mask
   * @return a new storage, masked with the given mask
   */
  public abstract Storage<T> mask(BitSet mask, int cardinality);

  /**
   * Returns a new storage, ordered according to the rules specified in a mask.
   *
   * @param mask@return a storage resulting from applying the reordering rules
   */
  public abstract Storage<T> applyMask(OrderMask mask);

  /**
   * Returns a new storage, resulting from applying the rules specified in a mask. The resulting
   * storage should contain the elements of the original storage, in the same order. However, the
   * number of consecutive copies of the i-th element of the original storage should be {@code
   * counts[i]}.
   *
   * @param counts the mask specifying elements duplication
   * @param total the sum of all elements in the mask, also interpreted as the size of the resulting
   *     storage
   * @return the storage masked according to the specified rules
   */
  public abstract Storage<T> countMask(int[] counts, int total);

  /** @return a copy of the storage containing a slice of the original data */
  public abstract Storage<T> slice(int offset, int limit);

  /**
   * @return a new storage instance, containing the same elements as this one, with {@code count}
   *     nulls appended at the end
   */
  public Storage<?> appendNulls(int count) {
    Builder builder = new InferredBuilder(size() + count);
    builder.appendBulkStorage(this);
    builder.appendNulls(count);
    return builder.seal();
  }

  /**
   * Creates a builder that is capable of creating storages of the same type as the current one.
   *
   * <p>This is useful for example when copying the current storage with some modifications.
   */
  public abstract Builder createDefaultBuilderOfSameType(int capacity);

  /** @return a copy of the storage consisting of slices of the original data */
  public abstract Storage<T> slice(List<SliceRange> ranges);

  public List<Object> toList() {
    return new StorageListView(this);
  }

  /**
   * Counts the number of times each value has been seen before in this storage.
   *
   * @return a storage counting the number of times each value in this one has been seen before.
   */
  public Storage<?> duplicateCount() {
    long[] data = new long[size()];
    HashMap<Object, Integer> occurenceCount = new HashMap<>();
    for (int i = 0; i < size(); i++) {
      var value = getItemBoxed(i);
      var count = occurenceCount.getOrDefault(value, 0);
      data[i] = count;
      occurenceCount.put(value, count + 1);
    }
    return new LongStorage(data);
  }
}
