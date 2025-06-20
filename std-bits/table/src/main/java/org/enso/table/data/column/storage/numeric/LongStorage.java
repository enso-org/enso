package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.type.IntegerType;

/** A column storing 64-bit integers. */
public final class LongStorage extends AbstractLongStorage implements ColumnStorageWithNothingMap {

  // TODO [RW] at some point we will want to add separate storage classes for byte, short and int,
  // for more compact storage and more efficient handling of smaller integers; for now we will be
  // handling this just by checking the bounds
  final long[] data;
  final BitSet isNothing;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   * @param isNothing a bit set denoting at index {@code i} whether or not the value at index {@code
   *     i} is missing.
   * @param type the type specifying the bit-width of integers that are allowed in this storage
   */
  public LongStorage(long[] data, int size, BitSet isNothing, IntegerType type) {
    super(size, type);
    this.data = data;
    this.isNothing = isNothing;
  }

  public LongStorage(long[] data, IntegerType type) {
    this(data, data.length, new BitSet(), type);
  }

  @Override
  public long getItemAsLong(long index) {
    return data[(int) index];
  }

  @Override
  public boolean isNothing(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return isNothing.get(Math.toIntExact(idx));
  }

  @Override
  public BitSet getIsNothingMap() {
    return isNothing;
  }

  /** Widening to a bigger type can be done without copying the data. */
  @Override
  public LongStorage widen(IntegerType widerType) {
    assert widerType.fits(getType());
    return new LongStorage(data, (int) getSize(), getIsNothingMap(), widerType);
  }

  /** Allow access to the underlying data array for copying. */
  public long[] getArray() {
    return data;
  }
}
