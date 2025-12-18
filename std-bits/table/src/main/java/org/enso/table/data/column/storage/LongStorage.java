package org.enso.table.data.column.storage;

import java.lang.foreign.MemorySegment;
import java.nio.LongBuffer;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.util.ImmutableBitSet;

/** A column storing 64-bit integers. */
public final class LongStorage extends AbstractLongStorage implements ColumnStorageWithValidityMap {
  // TODO [RW] at some point we will want to add separate storage classes for byte, short and int,
  // for more compact storage and more efficient handling of smaller integers; for now we will be
  // handling this just by checking the bounds
  private final LongBuffer data;
  private final ImmutableBitSet validityMap;

  /** original proxy storage to keep from being garbage collected */
  private final ColumnStorage<?> proxy;

  /**
   * @param data the underlying data up to {@code data.limit()}
   * @param validityMap a bit set denoting at index {@code i} whether or not the real value is
   *     present.
   * @param type the type specifying the bit-width of integers that are allowed in this storage
   * @param otherStorage reference to proxy storage to prevent it from being GCed while this storage
   *     is used
   */
  public LongStorage(
      LongBuffer data,
      ImmutableBitSet validityMap,
      IntegerType type,
      ColumnStorage<?> otherStorage) {
    super(data.limit(), type);
    this.data = data;
    this.validityMap = validityMap;
    this.proxy = otherStorage;
  }

  @Override
  public long addressOfData() {
    return MemorySegment.ofBuffer(data).address();
  }

  @Override
  public long addressOfValidity() {
    return MemorySegment.ofBuffer(validityMap.rawData()).address();
  }

  @Override
  public long getItemAsLong(long index) {
    return data.get((int) index);
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return !validityMap.get(Math.toIntExact(idx));
  }

  public ImmutableBitSet getValidityMap() {
    return validityMap;
  }

  /** Widening to a bigger type can be done without copying the data. */
  @Override
  public LongStorage widen(IntegerType widerType) {
    assert widerType.fits(getType());
    return new LongStorage(data, validityMap, widerType, proxy);
  }

  /**
   * Allow access to the underlying data array for copying.
   *
   * @return
   */
  public LongBuffer getData() {
    return data.asReadOnlyBuffer();
  }

  @Override
  public ColumnLongStorageIterator iteratorWithIndex() {
    return new LongStorageIterator(data.asReadOnlyBuffer(), validityMap);
  }

  private static final class LongStorageIterator implements ColumnLongStorageIterator {
    private final LongBuffer data;
    private final ImmutableBitSet validityMap;
    private int index = -1;

    LongStorageIterator(LongBuffer data, ImmutableBitSet validityMap) {
      this.data = data;
      this.validityMap = validityMap;
    }

    @Override
    public Long getItemBoxed() {
      return !validityMap.get(index) ? null : data.get(index);
    }

    @Override
    public long getItemAsLong() {
      return data.get(index);
    }

    @Override
    public boolean isNothing() {
      return !validityMap.get(index);
    }

    @Override
    public boolean hasNext() {
      return index + 1 < data.limit();
    }

    @Override
    public Long next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }
      index++;
      return getItemBoxed();
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
