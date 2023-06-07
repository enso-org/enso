package org.enso.table.data.column.storage;

import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.builder.object.ObjectBuilder;
import org.enso.table.data.column.operation.cast.CastProblemBuilder;
import org.enso.table.data.column.operation.cast.StorageConverter;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.graalvm.polyglot.Value;

import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

/** An abstract representation of a data column. */
public abstract class Storage<T> {
  /** @return the number of elements in this column (including NAs) */
  public abstract int size();

  /** @return the number of NA elements in this column */
  public abstract int countMissing();

  /** @return the type tag of this column's storage. */
  public abstract StorageType getType();

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
    public static final String TRUNCATE = "truncate";
    public static final String CEIL = "ceil";
    public static final String FLOOR = "floor";
    public static final String NOT = "not";
    public static final String AND = "&&";
    public static final String OR = "||";
    public static final String IS_NOTHING = "is_nothing";
    public static final String IS_NAN = "is_nan";
    public static final String IS_EMPTY = "is_empty";
    public static final String STARTS_WITH = "starts_with";
    public static final String ENDS_WITH = "ends_with";
    public static final String CONTAINS = "contains";
    public static final String LIKE = "like";
    public static final String IS_IN = "is_in";
    public static final String YEAR = "year";
    public static final String MONTH = "month";
    public static final String DAY = "day";
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
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @param problemBuilder a builder for reporting computation problems
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage<?> bimap(
      String name,
      BiFunction<Object, Object, Object> function,
      Object argument,
      boolean skipNulls,
      StorageType expectedResultType,
      MapOperationProblemBuilder problemBuilder) {
    if (name != null && isOpVectorized(name)) {
      return runVectorizedMap(name, argument, problemBuilder);
    }

    checkFallback(function, expectedResultType, name);

    Builder storageBuilder = Builder.getForType(expectedResultType, size());
    if (skipNulls && argument == null) {
      storageBuilder.appendNulls(size());
      return storageBuilder.seal();
    }

    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (skipNulls && it == null) {
        storageBuilder.appendNoGrow(null);
      } else {
        Object result = function.apply(it, argument);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        storageBuilder.appendNoGrow(converted);
      }
    }
    return storageBuilder.seal();
  }

  /**
   * Runs a function on each non-missing element in this storage and gathers the results.
   *
   * @param name a name of potential vectorized variant of the function that should be used if
   *     supported. If this argument is null, the vectorized operation will never be used.
   * @param function the function to run.
   * @param onMissing the value to place for missing cells, usually just null
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @param problemBuilder a builder for reporting computation problems
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage<?> map(
      String name,
      Function<Object, Value> function,
      Value onMissing,
      StorageType expectedResultType,
      MapOperationProblemBuilder problemBuilder) {
    if (name != null && isOpVectorized(name)) {
      return runVectorizedMap(name, null, problemBuilder);
    }

    checkFallback(function, expectedResultType, name);

    Object missingValue = Polyglot_Utils.convertPolyglotValue(onMissing);

    Builder storageBuilder = Builder.getForType(expectedResultType, size());
    for (int i = 0; i < size(); i++) {
      Object it = getItemBoxed(i);
      if (it == null) {
        storageBuilder.appendNoGrow(missingValue);
      } else {
        Value result = function.apply(it);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        storageBuilder.appendNoGrow(converted);
      }
    }
    return storageBuilder.seal();
  }

  /**
   * Runs a function on each pair of non-missing elements in this and arg.
   *
   * @param name a name of potential vectorized variant of the function that should be used if
   *     supported. If this argument is null, the vectorized operation will never be used.
   * @param function the function to run.
   * @param skipNa whether rows containing missing values should be passed to the function.
   * @param expectedResultType the expected type for the result storage; it is ignored if the
   *     operation is vectorized
   * @param problemBuilder the builder used for reporting computation problems
   * @return the result of running the function on all non-missing elements.
   */
  public final Storage<?> zip(
      String name,
      BiFunction<Object, Object, Object> function,
      Storage<?> arg,
      boolean skipNa,
      StorageType expectedResultType,
      MapOperationProblemBuilder problemBuilder) {
    if (name != null && isOpVectorized(name)) {
      return runVectorizedZip(name, arg, problemBuilder);
    }

    checkFallback(function, expectedResultType, name);

    Builder storageBuilder = Builder.getForType(expectedResultType, size());
    for (int i = 0; i < size(); i++) {
      Object it1 = getItemBoxed(i);
      Object it2 = i < arg.size() ? arg.getItemBoxed(i) : null;
      if (skipNa && (it1 == null || it2 == null)) {
        storageBuilder.appendNoGrow(null);
      } else {
        Object result = function.apply(it1, it2);
        Object converted = Polyglot_Utils.convertPolyglotValue(result);
        storageBuilder.appendNoGrow(converted);
      }
    }
    return storageBuilder.seal();
  }

  private static void checkFallback(Object fallback, StorageType storageType, String operationName)
      throws IllegalArgumentException {
    if (fallback == null) {
      if (operationName == null) {
        throw new IllegalArgumentException(
            "A function or name of vectorized operation must be specified. This is a bug in the Table library.");
      } else {
        throw new IllegalArgumentException(
            "The operation "
                + operationName
                + " has no vectorized implementation for this storage type, but no fallback function was provided. This is a bug in the Table library.");
      }
    }

    if (storageType == null) {
      throw new IllegalArgumentException(
          "The expected result type must be specified if a fallback function is used. This is a bug in the Table library.");
    }
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
   * @param commonType a common type that should fit values from both storages
   * @return a new storage with missing values filled
   */
  public Storage<?> fillMissingFrom(Storage<?> other, StorageType commonType) {
    var builder = Builder.getForType(commonType, size());
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
   * @param total the sum of all elements in the mask, also interpreted as the length of the
   *     resulting storage
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

  public final Storage<?> cast(StorageType targetType, CastProblemBuilder castProblemBuilder) {
    StorageConverter<?> converter = StorageConverter.fromStorageType(targetType);
    return converter.cast(this, castProblemBuilder);
  }
}
