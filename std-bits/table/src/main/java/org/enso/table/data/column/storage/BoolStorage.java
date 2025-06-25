package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.iterators.ColumnBooleanStorageIterator;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.StorageType;

/** A boolean column storage. */
public final class BoolStorage extends Storage<Boolean>
    implements ColumnBooleanStorage, ColumnStorageWithNothingMap {
  private final BitSet values;
  private final BitSet isNothing;
  private final int size;
  private final boolean negated;

  public BoolStorage(BitSet values, BitSet isNothing, int size, boolean negated) {
    this.values = values;
    this.isNothing = isNothing;
    this.size = size;
    this.negated = negated;
  }

  @Override
  public long getSize() {
    return size;
  }

  @Override
  public Boolean getItemBoxed(long idx) {
    return isNothing(idx) ? null : getItemAsBoolean(idx);
  }

  @Override
  public StorageType<Boolean> getType() {
    return BooleanType.INSTANCE;
  }

  @Override
  public boolean getItemAsBoolean(long index) throws ValueIsNothingException {
    if (isNothing(index)) {
      throw new ValueIsNothingException(index);
    }

    return negated != values.get((int) index);
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return isNothing.get((int) idx);
  }

  public boolean isNegated() {
    return negated;
  }

  public BitSet getValues() {
    return values;
  }

  @Override
  public BitSet getIsNothingMap() {
    return isNothing;
  }

  @Override
  public ColumnBooleanStorageIterator iteratorWithIndex() {
    return new BoolStorageIterator(this);
  }

  private static class BoolStorageIterator implements ColumnBooleanStorageIterator {
    private final BoolStorage parent;
    private int index = -1;

    public BoolStorageIterator(BoolStorage parent) {
      this.parent = parent;
    }

    @Override
    public Boolean getItemBoxed() {
      return parent.getItemBoxed(index);
    }

    @Override
    public boolean getItemAsBoolean() {
      return !parent.isNothing(index) && parent.getItemAsBoolean(index);
    }

    @Override
    public boolean isNothing() {
      return parent.isNothing(index);
    }

    @Override
    public boolean hasNext() {
      return index + 1 < parent.getSize();
    }

    @Override
    public Boolean next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }
      return parent.getItemBoxed(++index);
    }

    @Override
    public long getIndex() {
      return index;
    }

    @Override
    public boolean moveNext() {
      if (!hasNext()) {
        return false;
      }
      index++;
      return true;
    }
  }
}
