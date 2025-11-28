package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.LongBuffer;
import java.util.BitSet;
import java.util.Objects;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.BitSets;

/** A builder for integer columns. */
class LongBuilder extends NumericBuilder implements BuilderForLong, BuilderWithRetyping {
  protected final ProblemAggregator problemAggregator;
  private ByteBuffer whole;
  private LongBuffer data;
  private BitSet validityMap;

  protected LongBuilder(int initialSize, ProblemAggregator problemAggregator) {
    this(allocBuffer(initialSize, 0), problemAggregator);
  }

  private LongBuilder(Object[] bsAndLb, ProblemAggregator problemAggregator) {
    super(null);
    this.whole = (ByteBuffer) bsAndLb[0];
    this.validityMap = (BitSet) bsAndLb[1];
    this.data = (LongBuffer) bsAndLb[2];
    this.problemAggregator = problemAggregator;
  }

  static LongBuilder make(int initialSize, IntegerType type, ProblemAggregator problemAggregator) {
    if (type == null || type.equals(IntegerType.INT_64)) {
      return new LongBuilder(initialSize, problemAggregator);
    } else {
      return new BoundCheckedIntegerBuilder(initialSize, type, problemAggregator);
    }
  }

  static LongBuilder fromAddress(long address, long capacity, IntegerType type) {
    assert address != 0;
    var tripple = allocBuffer(capacity, address);
    var builder = new LongBuilder(tripple, null);
    return builder;
  }

  /**
   * Allocates continuous direct memory buffer. First of all there is a validity bit map (padded to
   * 8 bytes) followed by the actual data.
   *
   * @param initialSize the size of the buffer
   * @return tripple of whole {@link ByteBuffer}, {@link BitSet} and {@link LongBuffer}
   */
  private static Object[] allocBuffer(long initialSize, long address) {
    var rawValiditySize = Math.toIntExact(initialSize / 8 + 1);
    var roundedValiditySize = (rawValiditySize / 8 + 1) * 8;
    var wholeBytesSize = Math.toIntExact(roundedValiditySize + initialSize * Long.BYTES);
    ByteBuffer buf;
    if (address == 0L) {
      buf = ByteBuffer.allocateDirect(wholeBytesSize).order(ByteOrder.LITTLE_ENDIAN);
    } else {
      var seg = MemorySegment.ofAddress(address).reinterpret(wholeBytesSize);
      buf = seg.asByteBuffer().order(ByteOrder.LITTLE_ENDIAN);
    }
    assert buf.capacity() == wholeBytesSize;

    var endOfData = Math.toIntExact(initialSize * 8);
    var lb = buf.slice(0, endOfData).order(ByteOrder.LITTLE_ENDIAN).asLongBuffer();
    assert lb.capacity() == initialSize;
    assert lb.order() == ByteOrder.LITTLE_ENDIAN;
    var bb = buf.slice(endOfData, buf.capacity() - endOfData);
    var bs = BitSet.valueOf(bb);
    return new Object[] {buf, bs, lb};
  }

  @Override
  protected int getDataSize() {
    return data.capacity();
  }

  @Override
  protected void resize(int desiredCapacity) {
    var bsAndLb = allocBuffer(desiredCapacity, 0);
    var newBs = (BitSet) bsAndLb[1];
    var newData = (LongBuffer) bsAndLb[2];
    int toCopy = Math.min(currentSize, data.capacity());
    newData.put(0, data, 0, toCopy);
    data = newData;
    whole = (ByteBuffer) bsAndLb[0];
    newBs.or(this.validityMap);
    validityMap = newBs;
  }

  @Override
  public void copyDataTo(Object[] items) {
    for (int i = 0; i < currentSize; i++) {
      if (!validityMap.get(i)) {
        items[i] = null;
      } else {
        items[i] = data.get(i);
      }
    }
  }

  @Override
  public boolean canRetypeTo(StorageType<?> type) {
    return Objects.equals(type, FloatType.FLOAT_64)
        || Objects.equals(type, BigIntegerType.INSTANCE)
        || Objects.equals(type, BigDecimalType.INSTANCE);
  }

  @Override
  public Builder retypeTo(StorageType<?> type) {
    if (Objects.equals(type, BigIntegerType.INSTANCE)) {
      return BigIntegerBuilder.retypeFromLongBuilder(this, this.problemAggregator);
    } else if (Objects.equals(type, FloatType.FLOAT_64)) {
      return InferredDoubleBuilder.retypeFromLongBuilder(this, this.problemAggregator);
    } else if (Objects.equals(type, BigDecimalType.INSTANCE)) {
      return BigDecimalBuilder.retypeFromLongBuilder(this);
    } else {
      throw new UnsupportedOperationException();
    }
  }

  @Override
  public IntegerType getType() {
    return IntegerType.INT_64;
  }

  @Override
  public boolean accepts(Object o) {
    return NumericConverter.isCoercibleToLong(o);
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof IntegerType otherType) {
      if (getType().fits(otherType)) {
        if (storage instanceof LongStorage longStorage) {
          // A fast path for the same type (or compatible) - no conversions/checks needed.
          int n = (int) longStorage.getSize();
          ensureFreeSpaceFor(n);
          data.put(currentSize, longStorage.getData(), 0, n);
          BitSets.copy(longStorage.getIsNothingMap(), validityMap, currentSize, n);
          currentSize += n;
        } else {
          // No conversions needed, but we need to iterate over the items.
          var longStorage = otherType.asTypedStorage(storage);
          long n = longStorage.getSize();
          for (long i = 0; i < n; i++) {
            if (longStorage.isNothing(i)) {
              appendNulls(1);
            } else {
              appendLong(longStorage.getItemAsLong(i));
            }
          }
        }
      }
    } else if (storage instanceof ColumnBooleanStorage boolStorage) {
      long n = boolStorage.getSize();
      for (long i = 0; i < n; i++) {
        if (boolStorage.isNothing(i)) {
          appendNulls(1);
        } else {
          appendLong(boolStorage.getItemAsBoolean(i) ? 1L : 0L);
        }
      }
    } else if (storage.getType() instanceof NullType) {
      appendNulls(Math.toIntExact(storage.getSize()));
    } else {
      throw new StorageTypeMismatchException(getType(), storage.getType());
    }
  }

  /**
   * Append a new integer to this builder.
   *
   * @param value the integer to append
   */
  @Override
  public LongBuilder appendLong(long value) {
    ensureSpaceToAppend();
    this.validityMap.set(currentSize);
    this.data.put(currentSize++, value);
    return this;
  }

  @Override
  public boolean isNothing(long index) {
    if (index >= currentSize) {
      throw new IndexOutOfBoundsException();
    } else {
      return !validityMap.get((int) index);
    }
  }

  @Override
  public long getLong(long index) {
    if (index >= currentSize) {
      throw new IndexOutOfBoundsException();
    } else {
      return data.get((int) index);
    }
  }

  @Override
  public long getCurrentCapacity() {
    return data.capacity();
  }

  @Override
  public LongBuilder appendNulls(int count) {
    var end = currentSize + count;
    validityMap.set(currentSize, end);
    currentSize = end;
    return this;
  }

  @Override
  public LongBuilder append(Object o) {
    if (o == null) {
      validityMap.set(currentSize++, false);
      return this;
    }

    Long x = NumericConverter.tryConvertingToLong(o);
    if (x != null) {
      appendLong(x);
    } else {
      throw new ValueTypeMismatchException(getType(), o);
    }

    return this;
  }

  @Override
  public ColumnStorage<Long> seal() {
    return seal(null);
  }

  /**
   * Seals this buffer as copy of provided storage.
   *
   * @param otherStorage storage to copy size from if non-{@code null}
   * @return locally copied storage
   */
  final LongStorage seal(ColumnStorage<?> otherStorage) {
    if (otherStorage != null) {
      currentSize = Math.toIntExact(otherStorage.getSize());
    }
    var buf = data.asReadOnlyBuffer().position(0).limit(currentSize);
    var address = MemorySegment.ofBuffer(whole).address();
    return new LongStorage(address, buf, validityMap, getType(), otherStorage);
  }
}
