package org.enso.table.data.column.storage;

import java.lang.foreign.MemorySegment;
import java.util.BitSet;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.iterators.ColumnBooleanStorageIterator;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.util.ImmutableBitSet;

/** A boolean column storage. */
public final class BoolStorage extends Storage<Boolean>
    implements ColumnBooleanStorage, ColumnStorageWithValidityMap {
  private final ImmutableBitSet values;
  private final ImmutableBitSet validityMap;
  private final int size;
  private final boolean negated;
  private final ColumnStorage<?> proxy;

  public BoolStorage(BitSet values, BitSet validityMap, int size, boolean negated) {
    this(
        new ImmutableBitSet(values, size),
        new ImmutableBitSet(validityMap, size),
        size,
        negated,
        null);
  }

  public BoolStorage(
      ImmutableBitSet values,
      ImmutableBitSet validityMap,
      int size,
      boolean negated,
      ColumnStorage<?> other) {
    super(BooleanType.INSTANCE);
    this.values = values;
    this.validityMap = validityMap;
    this.size = size;
    this.negated = negated;
    this.proxy = other;
  }

  @Override
  public long addressOfData() {
    return MemorySegment.ofBuffer(values.rawData()).address();
  }

  @Override
  public long addressOfValidity() {
    return MemorySegment.ofBuffer(validityMap.rawData()).address();
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
    return !validityMap.get((int) idx);
  }

  public boolean isNegated() {
    return negated;
  }

  public ImmutableBitSet getValues() {
    return values;
  }

  @Override
  public ImmutableBitSet getValidityMap() {
    return validityMap;
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
