package org.enso.table.data.column.operation.cast;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.PreciseTypeOptions;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.BlackholeProblemAggregator;
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
    };
  }

  public static StorageType<?> inferPreciseType(Column column) {
    return inferPreciseType(column, PreciseTypeOptions.DEFAULT);
  }

  public static StorageType<?> inferPreciseType(Column column, PreciseTypeOptions options) {
    var columnStorage = column.getStorage();
    var storage = ColumnStorageWithInferredStorage.resolveStorage(columnStorage);

    return switch (storage.getType()) {
      case TextType textType -> inferTextType(columnStorage, options);
      case IntegerType integerType -> inferIntegerType(columnStorage, options);
      case FloatType floatType -> inferFloatType(columnStorage, options);
      case BigIntegerType bigIntegerType -> inferBigIntegerType(columnStorage, options);
      case BigDecimalType bigDecimalType -> bigDecimalType;
      default -> storage.getType();
    };
  }

  private static class TextAccumulator {
    private static final LeastRecentlyUsedCache<String, Long> graphemeLengthCache =
        new LeastRecentlyUsedCache<>(1000);

    private long count = 0;
    private long minLength = Long.MAX_VALUE;
    private long maxLength = Long.MIN_VALUE;

    public void accumulate(String item) {
      if (item == null) {
        return;
      }

      count++;
      long length = graphemeLengthCache.computeIfAbsent(item, Text_Utils::grapheme_length);
      minLength = Math.min(minLength, length);
      maxLength = Math.max(maxLength, length);
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
    StorageIterators.buildOverStorage(
        textType.asTypedStorage(columnStorage),
        false,
        textType.makeBuilder(0, BlackholeProblemAggregator.INSTANCE),
        (builder, index, item) -> accumulator.accumulate(item));

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

    public void accumulate(long item, boolean isNothing) {
      if (isNothing) {
        return;
      }

      count++;
      minValue = Math.min(minValue, item);
      maxValue = Math.max(maxValue, item);
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

    if (integerType.bits().toInteger() <= 16) {
      // If the type is already the smallest possible, we return it unchanged.
      return integerType;
    }

    // Build the min and max of values in the column.
    var accumulator = new LongAccumulator();
    StorageIterators.buildOverLongStorage(
        integerType.asTypedStorage(columnStorage),
        false,
        integerType.makeBuilder(0, BlackholeProblemAggregator.INSTANCE),
        (builder, index, item, isNothing) -> accumulator.accumulate(item, isNothing));

    return accumulator.resolveType();
  }

  private static StorageType<?> inferBigIntegerType(
      ColumnStorage<?> columnStorage, PreciseTypeOptions options) {
    if (!(columnStorage.getType() instanceof BigIntegerType bigIntegerType)) {
      throw new IllegalArgumentException(
          "Cannot infer integer type from non-integer storage: " + columnStorage.getType());
    }

    // Build the min and max of values in the column.
    try {
      var accumulator = new LongAccumulator();
      StorageIterators.buildOverStorage(
          bigIntegerType.asTypedStorage(columnStorage),
          false,
          bigIntegerType.makeBuilder(0, BlackholeProblemAggregator.INSTANCE),
          (builder, index, item) -> {
            if (item == null) {
              return;
            }
            accumulator.accumulate(item.longValueExact(), false);
          });

      return options.shrinkIntegers()
          ? accumulator.resolveType()
          : (accumulator.getCount() > 0 ? bigIntegerType : IntegerType.INT_64);
    } catch (ArithmeticException e) {
      // If we cannot convert the value to long, we return the original type.
      return bigIntegerType;
    }
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
    try {
      var accumulator = new LongAccumulator();
      StorageIterators.buildOverDoubleStorage(
          floatType.asTypedStorage(columnStorage),
          false,
          floatType.makeBuilder(0, BlackholeProblemAggregator.INSTANCE),
          (builder, index, item, isNothing) -> {
            if (isNothing) {
              return;
            }
            if (item % 1 != 0 && IntegerType.INT_64.fits(item)) {
              throw new ArithmeticException(
                  "Value is not a whole number or doesn't fit in a long: " + item);
            }
            accumulator.accumulate((long) item, isNothing);
          });

      return options.shrinkIntegers()
          ? accumulator.resolveType()
          : (accumulator.getCount() > 0 ? floatType : IntegerType.INT_64);
    } catch (ArithmeticException e) {
      // If we cannot convert the value to long, we return the original type.
      return floatType;
    }
  }

  private static class ObjectTypeAccumulator {
    private long count = 0;
    private StorageType<?> currentType = null;

    public void accumulate(Object item) {
      if (item == null) {
        return;
      }

      count++;
      var itemType = StorageType.forBoxedItem(item, PreciseTypeOptions.DEFAULT);
      if (currentType == null) {
        currentType = itemType;
      } else {
        currentType = reconcileTypes(currentType, itemType);
      }

      if (currentType instanceof AnyObjectType) {
        // If we have an AnyObjectType, we can stop accumulating.
        throw new IllegalArgumentException("Reached AnyObjectType during accumulation.");
      }
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
    try {
      StorageIterators.buildOverStorage(
          AnyObjectType.INSTANCE.asTypedStorage(columnStorage),
          false,
          AnyObjectType.INSTANCE.makeBuilder(0, BlackholeProblemAggregator.INSTANCE),
          (builder, index, item) -> accumulator.accumulate(item));
      return accumulator.getCurrentType();
    } catch (IllegalArgumentException e) {
      // Could not combine so return AnyObjectType
      return AnyObjectType.INSTANCE;
    }
  }
}
