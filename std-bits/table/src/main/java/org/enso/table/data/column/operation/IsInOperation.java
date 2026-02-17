package org.enso.table.data.column.operation;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.BitSet;
import java.util.HashSet;
import java.util.List;
import java.util.function.BiPredicate;
import java.util.function.Function;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.unary.NotOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.ColumnStorageWithValidityMap;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.util.ImmutableBitSet;

/**
 * The IsInOperation class provides a way to check if a value is in a set of values. It checks if
 * the condition is valid.
 */
public final class IsInOperation {
  /**
   * Checks if the operation can be applied to the given column.
   *
   * @param column the column to check
   * @return true if the operation can be applied, false otherwise
   */
  public static boolean isSupported(Column column) {
    var storage = ColumnStorageWithInferredStorage.resolveStorage(column);
    var storageType = storage.getType();
    return !(storageType instanceof AnyObjectType);
  }

  /**
   * Applies the `is_in` operation to the given column.
   *
   * @param left the column to apply the operation to
   * @param new_name the name of the new column
   * @param arg the list of values to check against
   * @param problemAggregator the problem aggregator to report problems to
   * @return a new column with the results of the operation
   */
  public static Column apply(
      Column left, String new_name, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg instanceof Column argColumn) {
      return apply(left, new_name, argColumn.asList(), problemAggregator);
    }

    if (!(arg instanceof List<?> list)) {
      throw new IllegalArgumentException("Argument to `is_in` must be a vector.");
    }

    if (list.isEmpty()) {
      return new Column(new_name, Builder.fromRepeatedItem(false, left.getSize()));
    }

    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    var result =
        switch (leftStorage.getType()) {
          case NullType nt -> Builder.makeEmpty(BooleanType.INSTANCE, leftStorage.getSize());
          case BooleanType bt ->
              applyBooleanIsIn(bt.asTypedStorage(leftStorage), list, problemAggregator);
          case DateType dt ->
              applySpecialized(
                  dt.asTypedStorage(leftStorage), list, dt::valueAsType, problemAggregator);
          case DateTimeType dtt ->
              applySpecialized(
                  dtt.asTypedStorage(leftStorage), list, dtt::valueAsType, problemAggregator);
          case TimeOfDayType todt ->
              applySpecialized(
                  todt.asTypedStorage(leftStorage), list, todt::valueAsType, problemAggregator);
          case TextType tt ->
              applySpecialized(
                  tt.asTypedStorage(leftStorage), list, tt::valueAsType, problemAggregator);
          case IntegerType it ->
              applySpecialized(
                  it.asTypedStorage(leftStorage),
                  list,
                  NumericConverter::tryConvertingToLong,
                  problemAggregator);
          case FloatType ft ->
              applySpecialized(
                  ft.asTypedStorage(leftStorage),
                  list,
                  NumericConverter::tryConvertingToDouble,
                  problemAggregator);
          case BigIntegerType bit ->
              applySpecialized(
                  bit.asTypedStorage(leftStorage), list, bit::valueAsType, problemAggregator);
          case BigDecimalType bdt ->
              applySpecialized(
                  bdt.asTypedStorage(leftStorage),
                  list,
                  o -> tryConvertingToBigDecimal(o, problemAggregator),
                  problemAggregator,
                  IsInOperation::containsBigDecimal);
          default ->
              throw new IllegalArgumentException(
                  "Unsupported StorageType for `is_in`: " + leftStorage.getType());
        };

    return new Column(new_name, result);
  }

  private static BigDecimal tryConvertingToBigDecimal(
      Object o, MapOperationProblemAggregator problemAggregator) {
    return switch (o) {
      case BigDecimal x -> x;
      case BigInteger x -> new BigDecimal(x);
      case Double x -> {
        problemAggregator.reportFloatingPointEquality(-1);
        yield BigDecimal.valueOf(x);
      }
      case Float x -> {
        problemAggregator.reportFloatingPointEquality(-1);
        yield BigDecimal.valueOf(x);
      }
      case Long x -> BigDecimal.valueOf(x);
      case Integer x -> BigDecimal.valueOf(x);
      case Short x -> BigDecimal.valueOf(x);
      case Byte x -> BigDecimal.valueOf(x);
      case null, default -> null;
    };
  }

  private static boolean containsBigDecimal(HashSet<BigDecimal> set, BigDecimal bigDecimal) {
    return set.contains(bigDecimal) || set.stream().anyMatch(b -> b.compareTo(bigDecimal) == 0);
  }

  /**
   * An optimized representation of the vector of values to match.
   *
   * <p>It indicates whether the vector contained a null value and contains a hashmap of the vector
   * elements for faster contains checks.
   */
  private record CompactRepresentation<T>(HashSet<T> uniqueValues, boolean hadNull) {
    public static <T> CompactRepresentation<T> create(
        List<?> arg,
        Function<Object, T> converter,
        MapOperationProblemAggregator problemAggregator) {
      boolean hadNull = false;
      boolean hadFloat = false;
      var uniqueValues = new HashSet<T>();
      for (Object o : arg) {
        if (o == null) {
          hadNull = true;
        } else {
          T typedValue = converter.apply(o);

          if (typedValue != null) {
            if (!hadFloat && (typedValue instanceof Double || typedValue instanceof Float)) {
              hadFloat = true;
              problemAggregator.reportFloatingPointEquality(-1);
            }

            uniqueValues.add(typedValue);
          }
        }
      }
      return new CompactRepresentation<>(uniqueValues, hadNull);
    }
  }

  private static <T> ColumnStorage<?> applySpecialized(
      ColumnStorage<T> storage,
      List<?> arg,
      Function<Object, T> converter,
      MapOperationProblemAggregator problemAggregator) {
    return applySpecialized(storage, arg, converter, problemAggregator, HashSet::contains);
  }

  private static <T> ColumnStorage<?> applySpecialized(
      ColumnStorage<T> storage,
      List<?> arg,
      Function<Object, T> converter,
      MapOperationProblemAggregator problemAggregator,
      BiPredicate<HashSet<T>, T> contains) {
    // Convert the List to a Set<T>
    var result = CompactRepresentation.create(arg, converter, problemAggregator);

    // If the set is empty, return a constant storage
    if (result.uniqueValues.isEmpty()) {
      return result.hadNull()
          ? Builder.makeEmpty(BooleanType.INSTANCE, storage.getSize())
          : Builder.fromRepeatedItem(false, storage.getSize());
    }

    // Scan the storage and build the result
    final boolean hadNullFinal = result.hadNull();
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForBoolean(storage.getSize()),
        (builder, index, value) -> {
          if (value instanceof Double || value instanceof Float) {
            problemAggregator.reportFloatingPointEquality(index);
          }

          if (contains.test(result.uniqueValues, value)) {
            builder.appendBoolean(true);
          } else if (hadNullFinal) {
            builder.appendNulls(1);
          } else {
            builder.appendBoolean(false);
          }
        });
  }

  private record BooleanFlags(boolean hadNull, boolean hadTrue, boolean hadFalse) {
    public static BooleanFlags of(List<?> arg) {
      boolean hadNull = false;
      boolean hadTrue = false;
      boolean hadFalse = false;

      for (Object o : arg) {
        if (o == null) {
          hadNull = true;
        } else if (o instanceof Boolean b) {
          if (b) {
            hadTrue = true;
          } else {
            hadFalse = true;
          }
        }

        if (hadNull && hadTrue && hadFalse) {
          break; // No need to continue if all flags are set
        }
      }

      return new BooleanFlags(hadNull, hadTrue, hadFalse);
    }
  }

  private static ColumnStorage<?> applyBooleanIsIn(
      ColumnBooleanStorage boolStorage,
      List<?> arg,
      MapOperationProblemAggregator problemAggregator) {
    // Process arg into flags for true, false, and null
    var flags = BooleanFlags.of(arg);

    // If neither true nor false were found, we can return an empty or constant storage
    if (!flags.hadTrue && !flags.hadFalse) {
      return flags.hadNull
          ? Builder.makeEmpty(BooleanType.INSTANCE, boolStorage.getSize())
          : Builder.fromRepeatedItem(false, boolStorage.getSize());
    }

    // Convert Size
    int checkedSize = Builder.checkSize(boolStorage.getSize());

    // If had both true and false, then return all true when not nothing
    if (flags.hadTrue && flags.hadFalse) {
      var validityMap = makeValidityMap(boolStorage, checkedSize);
      return new BoolStorage(
          ImmutableBitSet.allFalse(checkedSize), validityMap, checkedSize, true, null);
    }

    // Only have one of true or false
    if (!flags.hadNull) {
      return flags.hadTrue
          ? boolStorage
          : NotOperation.INSTANCE.apply(boolStorage, problemAggregator);
    }

    // Complicated case with nulls (hadNull is true)
    if (boolStorage instanceof BoolStorage specializedStorage) {
      return applyBoolStorage(flags.hadTrue, specializedStorage, checkedSize);
    }

    return StorageIterators.buildOverBooleanStorage(
        boolStorage,
        Builder.getForBoolean(boolStorage.getSize()),
        (builder, index, value, isNothing) -> {
          if (value == flags.hadTrue) {
            builder.appendBoolean(true);
          } else {
            builder.appendNulls(1);
          }
        });
  }

  private static ColumnStorage<?> applyBoolStorage(
      boolean keepValue, BoolStorage boolStorage, int checkedSize) {
    BitSet values = boolStorage.getValues().cloneBitSet();
    BitSet isNothing = boolStorage.getValidityMap().cloneBitSet();
    isNothing.flip(0, Math.toIntExact(boolStorage.getSize()));

    if (keepValue) {
      var newIsNothing =
          boolStorage.isNegated()
              ? or(isNothing, values, checkedSize)
              : orNot(isNothing, values, checkedSize);
      newIsNothing.flip(0, checkedSize);
      return new BoolStorage(values, newIsNothing, checkedSize, boolStorage.isNegated());
    } else {
      var newIsNothing =
          boolStorage.isNegated()
              ? orNot(isNothing, values, checkedSize)
              : or(isNothing, values, checkedSize);
      newIsNothing.flip(0, checkedSize);
      return new BoolStorage(values, newIsNothing, checkedSize, !boolStorage.isNegated());
    }
  }

  private static ImmutableBitSet makeValidityMap(ColumnStorage<?> storage, int size) {
    if (storage instanceof ColumnStorageWithValidityMap withNothingMap) {
      return withNothingMap.getValidityMap();
    }

    BitSet validityMap = new BitSet(size);
    for (int i = 0; i < size; i++) {
      if (!storage.isNothing(i)) {
        validityMap.set(i);
      }
    }
    return new ImmutableBitSet(validityMap, size);
  }

  private static BitSet or(BitSet left, BitSet right, int sizeIsIgnored) {
    BitSet result = (BitSet) left.clone();
    result.or(right);
    return result;
  }

  private static BitSet orNot(BitSet left, BitSet right, int size) {
    // Doing an extra operation to avoid doing an extra allocation.
    // a || !b => !(!a && b)
    BitSet result = (BitSet) left.clone();
    result.flip(0, size);
    result.and(right);
    result.flip(0, size);
    return result;
  }
}
