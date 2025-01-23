package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.type.IntegerType;
import org.graalvm.polyglot.Context;

/**
 * Implements a storage that computes the ith stored value using some function.
 *
 * <p>This storage allows for missing values. Prefer {@link ComputedLongStorage} for non-nullable
 * case.
 */
public abstract class ComputedNullableLongStorage extends AbstractLongStorage {
  protected abstract Long computeItem(int idx);

  protected ComputedNullableLongStorage(int size) {
    super(size, IntegerType.INT_64, null);
  }

  @Override
  public Long getBoxed(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(
          "Index " + idx + " is out of bounds for range of length " + getSize() + ".");
    }

    return computeItem((int) idx);
  }

  @Override
  public boolean isNothing(long idx) {
    return getBoxed(idx) == null;
  }

  @Override
  public long get(long idx) throws ValueIsNothingException {
    Long result = getBoxed(idx);
    if (result == null) {
      throw new ValueIsNothingException(idx);
    }
    return result;
  }

  @Override
  public BitSet getIsNothingMap() {
    if (isNothing == null) {
      // Only compute once as needed.
      BitSet newIsNothing = new BitSet();
      Context context = Context.getCurrent();
      for (int i = 0; i < getSize(); i++) {
        if (computeItem(i) == null) {
          newIsNothing.set(i);
        }

        context.safepoint();
      }
      isNothing = newIsNothing;
    }
    return isNothing;
  }

  @Override
  public AbstractLongStorage widen(IntegerType widerType) {
    // Currently the implementation only reports 64-bit type so there is no widening to do - we can
    // just return self.
    assert getType().equals(IntegerType.INT_64);
    return this;
  }
}
