package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.LongBuffer;
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

/** A builder for integer columns. */
sealed class LongBuilder extends NumericBuilder implements BuilderForLong, BuilderWithRetyping
    permits BoundCheckedIntegerBuilder {
  protected final ProblemAggregator problemAggregator;
  private LongBuffer data;

  protected LongBuilder(int initialSize, ProblemAggregator problemAggregator) {
    this(allocBuffer(initialSize, 0), initialSize, 0, problemAggregator);
  }

  private LongBuilder(
      LongBuffer data, int initialSize, long validity, ProblemAggregator problemAggregator) {
    super(initialSize, validity);
    this.data = data;
    this.problemAggregator = problemAggregator;
  }

  static LongBuilder make(int initialSize, IntegerType type, ProblemAggregator problemAggregator) {
    if (type == null || type.equals(IntegerType.INT_64)) {
      return new LongBuilder(initialSize, problemAggregator);
    } else {
      return new BoundCheckedIntegerBuilder(initialSize, type, problemAggregator);
    }
  }

  static LongBuilder fromAddress(int size, long address, long validity, IntegerType type) {
    assert address != 0;
    var buf = allocBuffer(size, address);
    var builder = new LongBuilder(buf, size, validity, null);
    return builder;
  }

  /**
   * Allocates continuous direct memory buffer.
   *
   * @param size the size of buffer to allocate
   * @param data address of data to read or {@code 0} to allocate new data
   * @return long buffer representing data
   */
  private static LongBuffer allocBuffer(int size, long data) {
    var wholeDataSize = Long.BYTES * size;
    ByteBuffer buf;
    if (data == 0L) {
      buf = ByteBuffer.allocateDirect(wholeDataSize).order(ByteOrder.LITTLE_ENDIAN);
    } else {
      var seg = MemorySegment.ofAddress(data).reinterpret(wholeDataSize);
      buf = seg.asByteBuffer().order(ByteOrder.LITTLE_ENDIAN);
    }
    assert buf.capacity() == wholeDataSize;
    var lb = buf.order(ByteOrder.LITTLE_ENDIAN).asLongBuffer();
    assert lb.capacity() == size;
    assert lb.order() == ByteOrder.LITTLE_ENDIAN;
    return lb;
  }

  @Override
  protected int getDataSize() {
    return data.capacity();
  }

  @Override
  protected void resize(int desiredCapacity) {
    var newData = allocBuffer(desiredCapacity, 0);
    int toCopy = Math.min(currentSize, data.capacity());
    newData.put(0, data, 0, toCopy);
    data = newData;
  }

  @Override
  public void copyDataTo(Object[] items) {
    for (int i = 0; i < currentSize; i++) {
      if (!isValid(i)) {
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
          appendValidityMap(longStorage.getValidityMap(), n);
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
    this.setValid(currentSize);
    this.data.put(currentSize++, value);
    return this;
  }

  @Override
  public boolean isNothing(long index) {
    if (index >= currentSize) {
      throw new IndexOutOfBoundsException();
    } else {
      return !isValid((int) index);
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
    doAppendNulls(count);
    return this;
  }

  @Override
  public LongBuilder append(Object o) {
    if (o == null) {
      doAppendNulls(1);
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
    return seal(null, getType());
  }

  /**
   * Seals this buffer as copy of provided storage.
   *
   * @param otherStorage storage to copy size from if non-{@code null}
   * @param type the type to assign to the created storage
   * @return locally copied storage
   */
  final LongStorage seal(ColumnStorage<?> otherStorage, IntegerType type) {
    ensureFreeSpaceFor(0);
    var buf = data.asReadOnlyBuffer().position(0).limit(currentSize);
    var validity = this.validityMap();
    return new LongStorage(buf, validity, type, otherStorage);
  }
}
