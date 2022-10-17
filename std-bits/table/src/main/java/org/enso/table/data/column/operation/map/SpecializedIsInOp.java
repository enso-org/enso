package org.enso.table.data.column.operation.map;

import java.util.BitSet;
import java.util.HashSet;
import java.util.List;
import java.util.function.Function;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;

/**
 * A specialized implementation for the IS_IN operation for builtin types, relying on hashing. Since
 * for some columns we know what types of objects can be stored, we can filter out any objects that
 * do not match that type and then rely on a consistent definition of hashcode for these builtin
 * types (which is not available in general for custom objects).
 */
public class SpecializedIsInOp<T, S extends Storage<T>> extends MapOperation<T, S> {
  /**
   * An optimized representation of the vector of values to match.
   *
   * <p>It indicates whether the vector contained a null value and contains a hashmap of the vector
   * elements for faster contains checks.
   */
  public record CompactRepresentation<T>(HashSet<T> coercedValues, boolean hasNulls) {}

  private final Function<List<?>, CompactRepresentation<T>> prepareList;

  /**
   * Creates a new operation with a given preprocessing function.
   *
   * <p>The responsibility of the function is to analyse the list and create a hashmap of relevant
   * elements, coerced to a type that is consistent with the storage type of the given column. Any
   * elements not fitting the expected type can (and should) be discarded.
   *
   * <p>It is important to correctly coerce the types, for example in Enso 2 == 2.0, so if we are
   * getting a Long for a DoubleColumn, it should be converted to a Double before adding it to the
   * hashmap. Similarly, for LongStorage, non-integer Doubles can be ignored, but Doubles with 0
   * fractional part need to be converted into a Long. These conversions can be achieved with the
   * {@code NumericConverter} class.
   */
  public static <T, S extends Storage<T>> SpecializedIsInOp<T, S> make(
      Function<List<?>, CompactRepresentation<T>> prepareList) {
    return new SpecializedIsInOp<>(prepareList);
  }

  /**
   * Creates a new operation which ensures the Enso Date/Time types are correctly coerced.
   *
   * <p>It uses the provided {@code storageClass} to only keep the elements that are of the same
   * type as expected in the storage.
   */
  public static <T, S extends Storage<T>> SpecializedIsInOp<T, S> makeForTimeColumns(Class<T> storageClass) {
    return SpecializedIsInOp.make(
        list -> {
          HashSet<T> set = new HashSet<>();
          boolean hasNulls = false;
          for (Object o : list) {
            hasNulls |= o == null;
            Object coerced = Polyglot_Utils.convertPolyglotValue(o);
            if (storageClass.isInstance(coerced)) {
              set.add(storageClass.cast(coerced));
            }
          }
          return new SpecializedIsInOp.CompactRepresentation<>(set, hasNulls);
        });
  }

  SpecializedIsInOp(Function<List<?>, CompactRepresentation<T>> prepareList) {
    super(Storage.Maps.IS_IN);
    this.prepareList = prepareList;
  }

  @Override
  public Storage<?> runMap(S storage, Object arg) {
    if (arg instanceof List) {
      return runMap(storage, (List<?>) arg);
    } else {
      throw new IllegalArgumentException("Argument to `is_in` must be a vector.");
    }
  }

  public Storage<?> runMap(S storage, List<?> arg) {
    CompactRepresentation<T> compactRepresentation = prepareList.apply(arg);
    BitSet newVals = new BitSet();
    for (int i = 0; i < storage.size(); i++) {
      if (storage.isNa(i) && compactRepresentation.hasNulls) {
        newVals.set(i);
      } else if (compactRepresentation.coercedValues.contains(storage.getItemBoxed(i))) {
        newVals.set(i);
      }
    }
    return new BoolStorage(newVals, new BitSet(), storage.size(), false);
  }

  @Override
  public Storage<?> runZip(S storage, Storage<?> arg) {
    throw new IllegalStateException("Zip mode is not supported for this operation.");
  }
}
