package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.util.ImmutableBitSet;

/**
 * Implements a storage that computes the ith stored value using some function.
 *
 * <p>This storage assumes that _all_ values are present.
 */
public abstract class ComputedLongStorage extends AbstractLongStorage
    implements ColumnStorageWithValidityMap {
  protected abstract long computeItem(long idx);

  protected ComputedLongStorage(int size) {
    super(size, IntegerType.INT_64);
  }

  @Override
  public long getItemAsLong(long index) throws ValueIsNothingException {
    if (index < 0 || index >= getSize()) {
      throw new IndexOutOfBoundsException(index);
    }
    return computeItem(index);
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return false;
  }

  @Override
  public ImmutableBitSet getValidityMap() {
    var size = Math.toIntExact(getSize());
    return ImmutableBitSet.allTrue(size);
  }

  @Override
  public AbstractLongStorage widen(IntegerType widerType) {
    // Currently the implementation only reports 64-bit type so there is no widening to do - we can
    // just return self.
    assert getType().equals(IntegerType.INT_64);
    return this;
  }
}
