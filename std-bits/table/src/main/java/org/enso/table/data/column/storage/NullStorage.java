package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.Iterator;
import java.util.List;
import java.util.stream.LongStream;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;

/** A specialized storage that can be used by columns that contain only null values. */
public class NullStorage extends Storage<Void> {
  private static final MapOperationStorage<Void, NullStorage> OPS = buildOps();

  private static MapOperationStorage<Void, NullStorage> buildOps() {
    MapOperationStorage<Void, NullStorage> ops = new MapOperationStorage<>();
    ops.add(new NullOp(Maps.MUL));
    ops.add(new NullOp(Maps.ADD));
    ops.add(new NullOp(Maps.SUB));
    ops.add(new NullOp(Maps.DIV));
    ops.add(new NullOp(Maps.MOD));
    ops.add(new NullOp(Maps.POWER));
    return ops;
  }

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
  protected Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemAggregator problemAggregator) {
    return OPS.runBinaryMap(name, this, argument, problemAggregator);
  }

  @Override
  protected Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemAggregator problemAggregator) {
    return OPS.runZip(name, this, argument, problemAggregator);
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
  public Storage<?> appendNulls(int count) {
    return new NullStorage(size + count);
  }

  @Override
  public Storage<Void> slice(List<SliceRange> ranges) {
    return new NullStorage(SliceRange.totalLength(ranges));
  }

  @Override
  public Storage<?> fillMissingFromPrevious(BoolStorage missingIndicator) {
    return this;
  }

  /** A binary operation that always returns null. */
  private static class NullOp extends BinaryMapOperation<Void, NullStorage> {
    public NullOp(String name) {
      super(name);
    }

    @Override
    public Storage<?> runBinaryMap(
        NullStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      // We return the same storage as-is, because all lhs arguments are guaranteed to be null.
      return storage;
    }

    @Override
    public Storage<?> runZip(
        NullStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
      // We return the same storage as-is, because all lhs arguments are guaranteed to be null.
      return storage;
    }
  }
}
