package org.enso.table.data.column.operation.map.bool;

import org.enso.base.Polyglot_Utils;
import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.graalvm.polyglot.Value;

import java.util.BitSet;
import java.util.HashSet;
import java.util.List;
import java.util.function.Function;

public class BooleanIsInOp<T extends BoolStorage> extends MapOperation<T> {
  public BooleanIsInOp() {
    super(Storage.Maps.IS_IN);
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
  public Storage runZip(T storage, Storage arg) {
    throw new IllegalStateException("Zip mode is not supported for this operation.");
  }
}
