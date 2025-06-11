package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.Iterator;
import java.util.List;
import java.util.stream.LongStream;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;

/** A specialized storage that can be used by columns that contain only null values. */
public class NullStorage extends Storage<Void> {
  private final long size;

  public NullStorage(long size) {
    this.size = size;
  }

  @Override
  public long getSize() {
    return size;
  }

  @Override
  public StorageType<Void> getType() {
    return NullType.INSTANCE;
  }

  @Override
  public boolean isNothing(long index) {
    if (index < 0 || index >= getSize()) {
      throw new IndexOutOfBoundsException(index);
    }
    return true;
  }

  @Override
  public Void getItemBoxed(long idx) {
    if (idx < 0 || idx >= getSize()) {
      throw new IndexOutOfBoundsException(idx);
    }
    return null;
  }

  @Override
  public Iterator<Void> iterator() {
    return LongStream.range(0, size).mapToObj(i -> (Void) null).iterator();
  }

  @Override
  public Storage<Void> applyFilter(BitSet filterMask, int newLength) {
    return new NullStorage(newLength);
  }

  @Override
  public Storage<Void> applyMask(OrderMask mask) {
    return new NullStorage(mask.length());
  }

  @Override
  public Storage<Void> slice(int offset, int limit) {
    long newSize = Math.min(this.size - offset, limit);
    return new NullStorage(newSize);
  }

  @Override
  public Storage<Void> slice(List<SliceRange> ranges) {
    return new NullStorage(SliceRange.totalLength(ranges));
  }

  @Override
  public Storage<?> fillMissingFromPrevious(BoolStorage missingIndicator) {
    return this;
  }
}
