package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.Storage;

import java.util.BitSet;

/** An operation that takes a single argument of some type and returns an integer. */
public abstract class UnaryIntegerOp<T, I extends Storage<T>> extends UnaryMapOperation<T, I> {

  public UnaryIntegerOp(String name) {
    super(name);
  }

  protected abstract long doOperation(T value);

  @Override
  protected Storage<?> run(I storage) {
    BitSet newMissing = new BitSet();
    long[] newVals = new long[storage.size()];
    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNa(i)) {
        newVals[i] = doOperation(storage.getItemBoxed(i));
      } else {
        newMissing.set(i);
      }
    }

    return new LongStorage(newVals, newVals.length, newMissing);
  }
}
