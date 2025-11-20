package org.enso.table.data.column.operation.cast;

import java.math.BigDecimal;
import java.util.Map;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.PreciseTypeOptions;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.LeastRecentlyUsedCache;

/** Perform a cast operation on a Column */
public class CastOperation {
  public static boolean canApply(StorageType<?> sourceType, StorageType<?> targetType) {
    if (targetType instanceof NullType) {
      return false;
    }

    var converter = fromStorageType(targetType);
    return converter.canApply(sourceType);
  }

  public static Column apply(
      Column source, StorageType<?> targetType, ProblemAggregator problemAggregator) {
    if (source.getStorage().getType().equals(targetType)) {
      return source;
    }

    var castProblemAggregator =
        new CastProblemAggregator(problemAggregator, source.getName(), targetType);
    var converter = fromStorageType(targetType);
    var newStorage = converter.cast(source.getStorage(), castProblemAggregator);

    return new Column(source.getName(), newStorage);
  }

  /** Construct a StorageConverter for the given target type. */
  private static StorageConverter<?> fromStorageType(StorageType<?> storageType) {
    return switch (storageType) {
      case AnyObjectType anyObjectType -> new ToMixedStorageConverter();
      case BooleanType booleanType -> new ToBooleanStorageConverter();
      case DateType dateType -> new ToDateStorageConverter();
      case DateTimeType dateTimeType -> new ToDateTimeStorageConverter();
      case FloatType floatType -> new ToFloatStorageConverter(floatType);
      case IntegerType integerType -> new ToIntegerStorageConverter(integerType);
      case TextType textType -> new ToTextStorageConverter(textType);
      case TimeOfDayType timeOfDayType -> new ToTimeOfDayStorageConverter();
      case BigIntegerType bigIntegerType -> new ToBigIntegerConverter();
      case BigDecimalType bigDecimalType -> new ToBigDecimalConverter();
      case NullType nullType -> throw new IllegalArgumentException("Cannot cast to Null type.");
      default ->
          fromStorageType(
              StorageType.fromTypeCharAndSize(storageType.typeChar(), storageType.size()));
    };
  }

  public static StorageType<?> inferPreciseType(Column column) {
    return inferPreciseType(column, PreciseTypeOptions.DEFAULT);
  }

  public static StorageType<?> inferPreciseType(Column column, PreciseTypeOptions options) {
    var columnStorage = column.getStorage();
    var storage = ColumnStorageWithInferredStorage.resolveStorage(columnStorage);

    return switch (storage.getType()) {
      case TextType textType -> inferTextType(storage, options);
      case IntegerType integerType -> inferIntegerType(storage, options);
      case FloatType floatType -> inferFloatType(storage, options);
      case BigIntegerType bigIntegerType -> inferBigIntegerType(storage, options);
      case BigDecimalType bigDecimalType -> inferBigDecimalType(storage, options);
      default -> storage.getType();
    };
  }

  private static class TextAccumulator {
    private static final Map<String, Long> graphemeLengthCache = new LeastRecentlyUsedCache<>(1000);

    private long count = 0;
    private long minLength = Long.MAX_VALUE;
    private long maxLength = Long.MIN_VALUE;

    public boolean accumulate(String item) {
      count++;
      long length = graphemeLengthCache.computeIfAbsent(item, Text_Utils::grapheme_length);
      minLength = Math.min(minLength, length);
      maxLength = Math.max(maxLength, length);
      return false;
    }

    public boolean allNull() {
      return count == 0;
    }

    public long getMaxLength() {
      return maxLength;
    }

    public long getMinLength() {
      return minLength;
    }
  }

  private static StorageType<?> inferTextType(
      ColumnStorage<?> columnStorage, PreciseTypeOptions options) {
    if (!options.shrinkText()) {
      return columnStorage.getType();
    }

    if (!(columnStorage.getType() instanceof TextType textType)) {
      throw new IllegalArgumentException(
          "Cannot infer text type from non-text storage: " + columnStorage.getType());
    }

    if (textType.fixedLength()) {
      return textType;
    }

    // Build the min and max length of the text values in the column.
    var accumulator = new TextAccumulator();
    StorageIterators.forEachOverStorage(
        textType.asTypedStorage(columnStorage),
        "inferTextType",
        (index, item) -> accumulator.accumulate(item));

    // Everything is null or empty, so return the original type.
    if (accumulator.allNull() || accumulator.getMaxLength() == 0) {
      return textType;
    }

    if (accumulator.getMinLength() == accumulator.getMaxLength()) {
      // If all the strings are of the same length, we can return a fixed-length type.
      return TextType.fixedLength(accumulator.getMinLength());
    }

    // If the strings are of varying lengths, we can return a variable-length type.
    // We will shrink it to a maximum of 255 characters if the original type was unbounded or larger
    // than 255 characters and all the strings fit into that bound.
    final long SHORT_LENGTH_THRESHOLD = 255;
    if ((accumulator.getMaxLength() <= SHORT_LENGTH_THRESHOLD)
        && (textType.maxLength() < 0 || textType.maxLength() > SHORT_LENGTH_THRESHOLD)) {
      return TextType.variableLengthWithLimit(SHORT_LENGTH_THRESHOLD);
    }

    return textType;
  }

  private static class LongAccumulator {
    private long count = 0;
    private long minValue = Long.MAX_VALUE;
    private long maxValue = Long.MIN_VALUE;

    public boolean accumulate(long item) {
      count++;
      minValue = Math.min(minValue, item);
      maxValue = Math.max(maxValue, item);
      return false;
    }

    public long getCount() {
      return count;
    }

    public IntegerType resolveType() {
      // Everything is null or all fits in INT_16
      if (count == 0 || (IntegerType.INT_16.fits(minValue) && IntegerType.INT_16.fits(maxValue))) {
        return IntegerType.INT_16;
      }

      // If all the values fit into INT_32, we can return that type.
      if (IntegerType.INT_32.fits(minValue) && IntegerType.INT_32.fits(maxValue)) {
        return IntegerType.INT_32;
      }

      // Otherwise, we return the original type.
      return IntegerType.INT_64;
    }
  }

  private static StorageType<?> inferIntegerType(
      ColumnStorage<?> columnStorage, PreciseTypeOptions options) {
    if (!options.shrinkIntegers()) {
      return columnStorage.getType();
    }

    if (!(columnStorage.getType() instanceof IntegerType integerType)) {
      throw new IllegalArgumentException(
          "Cannot infer integer type from non-integer storage: " + columnStorage.getType());
    }

    if (integerType.size() <= 16) {
      // If the type is already the smallest possible, we return it unchanged.
      return integerType;
    }

    // Build the min and max of values in the column.
    var accumulator = new LongAccumulator();
    StorageIterators.forEachOverLongStorage(
        integerType.asTypedStorage(columnStorage),
        "inferIntegerType",
        (index, item, isNothing) -> accumulator.accumulate(item));

    return accumulator.resolveType();
  }

  private static StorageType<?> inferBigIntegerType(
      ColumnStorage<?> columnStorage, PreciseTypeOptions options) {
    if (!(columnStorage.getType() instanceof BigIntegerType bigIntegerType)) {
      throw new IllegalArgumentException(
          "Cannot infer integer type from non-integer storage: " + columnStorage.getType());
    }

    // Build the min and max of values in the column.
    var accumulator = new LongAccumulator();
    var endedEarly =
        StorageIterators.forEachOverStorage(
            bigIntegerType.asTypedStorage(columnStorage),
            "inferBigIntegerType",
            (index, item) -> {
              try {
                return accumulator.accumulate(item.longValueExact());
              } catch (ArithmeticException e) {
                // If we cannot convert the value to long, we end early.
                return true; // This will stop the iteration.
              }
            });

    if (endedEarly) {
      // If we ended early, it means we encountered a value that could not be converted to long.
      // We return the original type.
      return bigIntegerType;
    }

    return options.shrinkIntegers()
        ? accumulator.resolveType()
        : (accumulator.getCount() == 0 ? bigIntegerType : IntegerType.INT_64);
  }

  private static StorageType<?> inferFloatType(
      ColumnStorage<?> columnStorage, PreciseTypeOptions options) {
    if (!options.wholeFloatsBecomeIntegers()) {
      return columnStorage.getType();
    }

    if (!(columnStorage.getType() instanceof FloatType floatType)) {
      throw new IllegalArgumentException(
          "Cannot infer float type from non-integer storage: " + columnStorage.getType());
    }

    // Build the min and max of values in the column.
    var accumulator = new LongAccumulator();
    var endedEarly =
        StorageIterators.forEachOverDoubleStorage(
            floatType.asTypedStorage(columnStorage),
            "inferFloatType",
            (index, item, isNothing) -> {
              if (item % 1 != 0 || !IntegerType.INT_64.fits(item)) {
                // If the value is not a whole number or does not fit in a long, we end early.
                return true; // This will stop the iteration.
              }
              return accumulator.accumulate((long) item);
            });

    if (endedEarly) {
      // If we ended early, it means we encountered a value that could not be converted to long.
      // We return the original type.
      return floatType;
    }

    return accumulator.getCount() == 0
        ? floatType
        : (options.shrinkIntegers() ? accumulator.resolveType() : IntegerType.INT_64);
  }

  private static class BigDecimalAccumulator extends LongAccumulator {
    private boolean overflowed = false;

    public boolean accumulate(BigDecimal item) {
      try {
        var bigIntegerValue = item.toBigIntegerExact();
        if (!overflowed) {
          if (IntegerType.INT_64.fits(bigIntegerValue)) {
            super.accumulate(bigIntegerValue.longValueExact());
          } else {
            overflowed = true;
          }
        }
        return overflowed;
      } catch (ArithmeticException e) {
        // If we cannot convert the value to long, we mark it as overflowed.
        overflowed = true;
        return true; // This will stop the iteration.
      }
    }

    public boolean getOverflowed() {
      return overflowed;
    }
  }

  private static StorageType<?> inferBigDecimalType(
      ColumnStorage<?> columnStorage, PreciseTypeOptions options) {
    if (!options.wholeFloatsBecomeIntegers()) {
      return columnStorage.getType();
    }

    if (!(columnStorage.getType() instanceof BigDecimalType bigDecimalType)) {
      throw new IllegalArgumentException(
          "Cannot infer decimal type from non-decimal storage: " + columnStorage.getType());
    }

    // Build the min and max of values in the column.
    var accumulator = new BigDecimalAccumulator();
    StorageIterators.forEachOverStorage(
        bigDecimalType.asTypedStorage(columnStorage),
        "inferBigDecimalType",
        (index, item) -> accumulator.accumulate(item));

    if (accumulator.getOverflowed() || accumulator.getCount() == 0) {
      // If there are no items or it overflowed, we return the original type.
      return bigDecimalType;
    }

    // Will fit in a long, so we can return an IntegerType.
    return options.shrinkIntegers() ? accumulator.resolveType() : IntegerType.INT_64;
  }

  private static class ObjectTypeAccumulator {
    private long count = 0;
    private StorageType<?> currentType = null;

    public boolean accumulate(Object item) {
      count++;
      var itemType = StorageType.forBoxedItem(item, PreciseTypeOptions.DEFAULT);
      if (currentType == null) {
        currentType = itemType;
      } else {
        currentType = reconcileTypes(currentType, itemType);
      }

      // If we have an AnyObjectType, we can stop accumulating.
      return currentType instanceof AnyObjectType;
    }

    public StorageType<?> getCurrentType() {
      if (currentType == null) {
        return AnyObjectType.INSTANCE; // If no items were accumulated, return AnyObjectType.
      }
      return currentType;
    }

    private static StorageType<?> reconcileTypes(
        StorageType<?> currentType, StorageType<?> itemType) {
      if (currentType.equals(itemType)) {
        return currentType;
      } else {
        if (currentType instanceof TextType currentTextType
            && itemType instanceof TextType itemTextType) {
          return TextType.maxType(currentTextType, itemTextType);
        } else if (currentType.isNumeric() && itemType.isNumeric()) {
          return commonNumericType(currentType, itemType);
        } else {
          return AnyObjectType.INSTANCE;
        }
      }
    }

    private static StorageType<?> commonNumericType(StorageType<?> a, StorageType<?> b) {
      assert a.isNumeric();
      assert b.isNumeric();
      if (a instanceof BigDecimalType || b instanceof BigDecimalType) {
        return BigDecimalType.INSTANCE;
      } else if (a instanceof FloatType || b instanceof FloatType) {
        return FloatType.FLOAT_64;
      } else if (a instanceof BigIntegerType || b instanceof BigIntegerType) {
        return BigIntegerType.INSTANCE;
      } else {
        if (a instanceof IntegerType aInt && b instanceof IntegerType bInt) {
          return IntegerType.commonType(aInt, bInt);
        } else {
          throw new IllegalStateException("Unexpected numeric types: " + a + " and " + b);
        }
      }
    }
  }

  public static StorageType<?> reconcileObjectStorage(ColumnStorage<?> columnStorage) {
    if (!(columnStorage.getType() instanceof AnyObjectType)) {
      return columnStorage.getType();
    }

    // Need to scan the column to determine the most appropriate type.
    var accumulator = new ObjectTypeAccumulator();
    StorageIterators.forEachOverStorage(
        AnyObjectType.INSTANCE.asTypedStorage(columnStorage),
        "reconcileObjectStorage",
        (index, item) -> accumulator.accumulate(item));
    return accumulator.getCurrentType();
  }

  private static class PrecisionAccumulator {
    private long maxPrecision = 0;

    public boolean accumulate(BigDecimal item) {
      int precision = item.precision();
      if (precision > maxPrecision) {
        maxPrecision = precision;
      }
      return false;
    }

    public long getMaxPrecision() {
      return maxPrecision;
    }
  }

  /**
   * Computes the maximum precision of the stored values in the column.
   *
   * @param column the column to analyze
   * @return the maximum precision of the stored values
   */
  public static long maxPrecisionStored(Column column) {
    var storage = column.getStorage();

    var accumulator = new PrecisionAccumulator();
    switch (storage.getType()) {
      case BigDecimalType bigDecimalType ->
          StorageIterators.forEachOverStorage(
              bigDecimalType.asTypedStorage(storage),
              "maxPrecisionStored:BigDecimal",
              (index, item) -> accumulator.accumulate(item));
      case BigIntegerType bigIntegerType ->
          StorageIterators.forEachOverStorage(
              bigIntegerType.asTypedStorage(storage),
              "maxPrecisionStored:BigInteger",
              (index, item) -> accumulator.accumulate(new BigDecimal(item)));
      default ->
          throw new IllegalArgumentException(
              "Cannot compute max precision for storage type: " + storage.getType());
    }

    return accumulator.getMaxPrecision();
  }
}
