package org.enso.table.data.column.operation.map.bool;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;

/**
 * A specialized implementation for the IS_IN operation on booleans - since booleans have just three
 * possible values we can have a highly efficient implementation that does not even rely on hashmap
 * and after processing the input vector, performs the checks in constant time.
 */
public class BooleanIsInOp extends MapOperation<BoolStorage> {
  public BooleanIsInOp() {
    super(Storage.Maps.IS_IN);
  }

  @Override
  public Storage runMap(BoolStorage storage, Object arg) {
    if (arg instanceof List) {
      return runMap(storage, (List<?>) arg);
    } else {
      throw new IllegalArgumentException("Argument to `is_in` must be a vector.");
    }
  }

  public Storage runMap(BoolStorage storage, List<?> arg) {
    boolean hadTrue = false;
    boolean hadFalse = false;
    boolean hadNull = false;

    for (Object o : arg) {
      switch (o) {
        case Boolean b -> {
          hadTrue |= b;
          hadFalse |= !b;
        }
        case null -> hadNull = true;
        default -> {}
      }
    }

    BitSet newVals = new BitSet();
    BitSet newMissing = new BitSet();
    for (int i = 0; i < storage.size(); i++) {
      if (storage.isNa(i)) {
        if (hadNull) {
          newVals.set(i);
        }
      } else {
        boolean val = storage.getItem(i);
        if ((val && hadTrue) || (!val && hadFalse)) {
          newVals.set(i);
        }
      }
    }
    return new BoolStorage(newVals, newMissing, storage.size(), false);
  }

  @Override
  public Storage runZip(BoolStorage storage, Storage arg) {
    throw new IllegalStateException("Zip mode is not supported for this operation.");
  }
}
