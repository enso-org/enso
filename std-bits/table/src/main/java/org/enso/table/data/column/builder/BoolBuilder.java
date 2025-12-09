package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.util.BitSet;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.util.ImmutableBitSet;

/** A builder for boolean columns. */
final class BoolBuilder implements BuilderForBoolean, BuilderWithRetyping {
  private final BitSet vals;
  private final BitSet validityMap;
  private int size;

  // ** Creates a new builder for boolean columns. Should be built via Builder.getForBoolean. */
  BoolBuilder(int capacity) {
    this(new BitSet(capacity), new BitSet(capacity), 0);
  }

  private BoolBuilder(BitSet vals, BitSet validityMap, int size) {
    this.vals = vals;
    this.validityMap = validityMap;
    this.size = size;
  }

  static BoolBuilder fromAddress(int sizeInBits, long data, long validity) {
    var bytesSize = (sizeInBits + 7) / 8;
    var vals = BitSet.valueOf(MemorySegment.ofAddress(data).reinterpret(bytesSize).asByteBuffer());
    var validityMap =
        BitSet.valueOf(MemorySegment.ofAddress(validity).reinterpret(bytesSize).asByteBuffer());
    return new BoolBuilder(vals, validityMap, sizeInBits);
  }

  @Override
  public BoolBuilder append(Object o) {
    if (o == null) {
      appendNulls(1);
    } else {
      if (o instanceof Boolean b) {
        if (b) {
          vals.set(size);
        }
        validityMap.set(size);
      } else {
        throw new ValueTypeMismatchException(getType(), o);
      }
      size++;
    }

    return this;
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof Boolean;
  }

  /**
   * Append a new boolean to this builder.
   *
   * @param value the boolean to append
   */
  public BoolBuilder appendBoolean(boolean value) {
    if (value) {
      vals.set(size);
    }
    validityMap.set(size, true);
    size++;
    return this;
  }

  @Override
  public BoolBuilder appendNulls(int count) {
    validityMap.set(size, size + count, false);
    size += count;
    return this;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    if (storage instanceof BoolStorage boolStorage) {
      // We know this is valid for a BoolStorage.
      int toCopy = (int) boolStorage.getSize();
      boolStorage.getValues().copyTo(vals, size, toCopy);
      boolStorage.getValidityMap().copyTo(validityMap, size, toCopy);
      size += toCopy;
    } else if (storage instanceof ColumnBooleanStorage columnBooleanStorage) {
      for (long i = 0; i < columnBooleanStorage.getSize(); i++) {
        if (columnBooleanStorage.isNothing(i)) {
          appendNulls(1);
        } else {
          appendBoolean(columnBooleanStorage.getItemAsBoolean(i));
        }
      }
    } else if (storage.getType() instanceof NullType) {
      appendNulls(Math.toIntExact(storage.getSize()));
    } else {
      throw new StorageTypeMismatchException(getType(), storage.getType());
    }
  }

  @Override
  public ColumnStorage<Boolean> seal() {
    return seal(null);
  }

  final ColumnStorage<Boolean> seal(ColumnStorage<?> other) {
    var vals = new ImmutableBitSet(this.vals, size);
    var validityMap = new ImmutableBitSet(this.validityMap, size);
    return new BoolStorage(vals, validityMap, size, false, other);
  }

  @Override
  public long getCurrentSize() {
    return size;
  }

  @Override
  public void copyDataTo(Object[] items) {
    for (int i = 0; i < size; i++) {
      if (!validityMap.get(i)) {
        items[i] = null;
      } else {
        items[i] = vals.get(i);
      }
    }
  }

  @Override
  public boolean canRetypeTo(StorageType<?> type) {
    return false;
  }

  @Override
  public Builder retypeTo(StorageType<?> type) {
    throw new UnsupportedOperationException();
  }

  @Override
  public StorageType<Boolean> getType() {
    return BooleanType.INSTANCE;
  }
}
