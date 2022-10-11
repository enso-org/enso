package org.enso.table.data.column.operation.map;

import org.enso.base.Polyglot_Utils;
import org.enso.table.data.NumericConverter;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.graalvm.polyglot.Value;

import java.util.BitSet;
import java.util.HashSet;
import java.util.List;
import java.util.function.Function;

public class SpecializedIsInOp<T extends Storage> extends MapOperation<T> {
  public record CompactRepresentation(HashSet<Object> coercedValues, boolean hasNulls) {}
  private final Function<List<?>, CompactRepresentation> prepareList;

  public static <U extends Storage> SpecializedIsInOp<U> make(Function<List<?>, CompactRepresentation> prepareList) {
    return new SpecializedIsInOp<>(prepareList);
  }

  public static <U extends Storage> SpecializedIsInOp<U> makeForTimeColumns() {
    return SpecializedIsInOp.make(list -> {
      HashSet<Object> set = new HashSet<>();
      boolean hasNulls = false;
      for (Object o : list) {
        hasNulls |= o == null;
        Object coerced = Polyglot_Utils.convertPolyglotValue(o);
        if (coerced != null) {
          set.add(coerced);
        }
      }
      return new SpecializedIsInOp.CompactRepresentation(set, hasNulls);
    });
  }

  SpecializedIsInOp(Function<List<?>, CompactRepresentation> prepareList) {
    super(Storage.Maps.IS_IN);
    this.prepareList = prepareList;
  }

  @Override
  public Storage runMap(T storage, Object arg) {
    if (arg instanceof List) {
      return runMap(storage, (List<?>) arg);
    } else {
      throw new IllegalArgumentException("Argument to `is_in` must be a vector.");
    }
  }

  public Storage runMap(T storage, List<?> arg) {
    CompactRepresentation compactRepresentation = prepareList.apply(arg);
    BitSet newVals = new BitSet();
    // TODO handling missing
    BitSet newMissing = new BitSet();
    for (int i = 0; i < storage.size(); i++) {
      if (storage.isNa(i) && compactRepresentation.hasNulls) {
        newVals.set(i);
      } else if (compactRepresentation.coercedValues.contains(storage.getItemBoxed(i))) {
        newVals.set(i);
      }
    }
    return new BoolStorage(newVals, newMissing, storage.size(), false);
  }

  @Override
  public Storage runZip(Storage storage, Storage arg) {
    throw new IllegalStateException("Zip mode is not supported for this operation.");
  }
}
