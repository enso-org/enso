package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.unary.NotOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StorageListView;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;

import java.util.BitSet;
import java.util.HashSet;
import java.util.List;

/**
 * The IsInOperation class provides a way to check if a value is in a set of values.
 * It checks if the condition is valid.
 */
public final class IsInOperation {
  /**
   * Checks if the operation can be applied to the given column.
   *
   * @param column the column to check
   * @return true if the operation can be applied, false otherwise
   */
  public static boolean isSupported(Column column) {
    var storage = BinaryOperation.getInferredStorage(column);
    var storageType = storage.getType();

    return storageType instanceof NullType
        || storageType instanceof BooleanType;
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
      Column left,
      String new_name,
      Object arg,
      MapOperationProblemAggregator problemAggregator) {
    if (arg instanceof Column argColumn) {
      var argStorage = BinaryOperation.getInferredStorage(argColumn);
      var argAsList = new StorageListView(argStorage);
      return apply(left, new_name, argAsList, problemAggregator);
    }

    if (!(arg instanceof List<?> list)) {
      throw new IllegalArgumentException("Argument to `is_in` must be a vector.");
    }

    var leftStorage = BinaryOperation.getInferredStorage(left);
    var result = switch (leftStorage.getType()) {
      case NullType nt -> BoolBuilder.makeEmpty(leftStorage.getSize());
      case BooleanType bt -> applyBooleanIsIn(bt.asTypedStorage(leftStorage), list, problemAggregator);
      case DateType dt -> applySpecialized(dt.asTypedStorage(leftStorage), list, dt, problemAggregator);
      default ->
          throw new IllegalArgumentException(
              "Unsupported StorageType for `is_in`: " + leftStorage.getType());
    };

    return new Column(new_name, (Storage<?>) result);
  }

  private static <T> ColumnStorage<?> applySpecialized(
      ColumnStorage<T> storage,
      List<?> arg,
      StorageType<T> valueType,
      MapOperationProblemAggregator problemAggregator) {
    // Convert the List to a Set<T>
    boolean hadNull = false;
    var uniqueValues = new HashSet<>();
    for (Object o : arg) {
      if (o == null) {
        hadNull = true;
      } else {
        T typedValue = valueType.valueAsType(o);
        if (typedValue != null) {
          uniqueValues.add(typedValue);
        }
      }
    }

    // If the set is empty, return a constant storage
    if (uniqueValues.isEmpty()) {
      return hadNull
          ? BoolBuilder.makeEmpty(storage.getSize())
          : BoolBuilder.makeConstant(storage.getSize(), false);
    }

    // Scan the storage and build the result
    final boolean hadNullFinal = hadNull;
    return StorageIterators.buildOverStorage(
        storage,
        Builder.getForBoolean(storage.getSize()),
        (builder, index, value) -> {
          if (uniqueValues.contains(value)) {
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

  private static ColumnStorage<?> applyBooleanIsIn(ColumnBooleanStorage boolStorage, List<?> arg, MapOperationProblemAggregator problemAggregator) {
    // Process arg into flags for true, false, and null
    var flags = BooleanFlags.of(arg);

    // If neither true nor false were found, we can return an empty or constant storage
    if (!flags.hadTrue && !flags.hadFalse) {
      return flags.hadNull
          ? BoolBuilder.makeEmpty(boolStorage.getSize())
          : BoolBuilder.makeConstant(boolStorage.getSize(), false);
    }

    // Convert Size
    int checkedSize = Builder.checkSize(boolStorage.getSize());

    // If had both true and false, then return all true when not nothing
    if (flags.hadTrue && flags.hadFalse) {
      var isNothing = makeIsNothingMap(boolStorage, checkedSize);
      return new BoolStorage(new BitSet(), isNothing, checkedSize, true);
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

  private static ColumnStorage<?> applyBoolStorage(boolean keepValue, BoolStorage boolStorage, int checkedSize) {
    BitSet values = boolStorage.getValues();
    BitSet isNothing = boolStorage.getIsNothingMap();

    if (keepValue) {
      var newIsNothing = boolStorage.isNegated() ? or(isNothing, values) : orNot(isNothing, values, checkedSize);
      return new BoolStorage(values, newIsNothing, checkedSize, boolStorage.isNegated());
    } else {
      var newIsNothing = boolStorage.isNegated() ? orNot(isNothing, values, checkedSize) : or(isNothing, values);
      return new BoolStorage(values, newIsNothing, checkedSize, !boolStorage.isNegated());
    }
  }

  private static BitSet makeIsNothingMap(ColumnStorage<?> storage, int size) {
    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return withNothingMap.getIsNothingMap();
    }

    BitSet isNothingMap = new BitSet(size);
    for (int i = 0; i < size; i++) {
      if (storage.isNothing(i)) {
        isNothingMap.set(i);
      }
    }
    return isNothingMap;
  }

  private static BitSet or(BitSet left, BitSet right) {
    BitSet result = (BitSet)left.clone();
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
