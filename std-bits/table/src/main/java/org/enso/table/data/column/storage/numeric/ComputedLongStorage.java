package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.type.IntegerType;

/**
 * Implements a storage that computes the ith stored value using some function.
 *
 * <p>This storage assumes that _all_ values are present.
 */
public abstract class ComputedLongStorage extends AbstractLongStorage {
  private static final BitSet EMPTY = new BitSet();

  protected abstract long computeItem(int idx);

  protected ComputedLongStorage(int size) {
    super(size, IntegerType.INT_64, EMPTY);
  }

  @Override
  public long get(long index) throws ValueIsNothingException {
    return computeItem((int) index);
  }

  @Override
  public AbstractLongStorage widen(IntegerType widerType) {
    // Currently the implementation only reports 64-bit type so there is no widening to do - we can
    // just return self.
    assert getType().equals(IntegerType.INT_64);
    return this;
  }
}
