package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;

import java.util.BitSet;

/** An operation that takes a single double argumebnt and returns a long. */
public abstract class UnaryLongToLongOp extends UnaryMapOperation<Long, AbstractLongStorage> {

  public UnaryLongToLongOp(String name) {
    super(name);
  }

  protected abstract long doOperation(long value);

  @Override
  protected LongStorage run(AbstractLongStorage storage) {
    BitSet newMissing = new BitSet();
    long[] newVals = new long[storage.size()];
    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNa(i)) {
        newVals[i] = doOperation(storage.getItem(i));
      } else {
        newMissing.set(i);
      }
    }

    return new LongStorage(newVals, newVals.length, newMissing);
  }
}
