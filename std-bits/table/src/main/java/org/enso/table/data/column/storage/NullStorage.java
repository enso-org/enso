package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForBoolean;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Value;

/** A specialized storage that can be used by columns that contain only null values. */
public class NullStorage extends Storage<Void> {
  private final long size;
  private final MapOperationStorage<Void, NullStorage> ops = buildOps();

  public NullStorage(long size) {
    this.size = size;
  }

  @Override
  public long getSize() {
    return size;
  }

  @Override
  public StorageType getType() {
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

  private static MapOperationStorage<Void, NullStorage> buildOps() {
    MapOperationStorage<Void, NullStorage> ops = new MapOperationStorage<>();
    ops.add(new NullOp(Maps.EQ));
    ops.add(new NullOp(Maps.LT));
    ops.add(new NullOp(Maps.LTE));
    ops.add(new NullOp(Maps.GT));
    ops.add(new NullOp(Maps.GTE));

    ops.add(new NullOp(Maps.MUL));
    ops.add(new NullOp(Maps.ADD));
    ops.add(new NullOp(Maps.SUB));
    ops.add(new NullOp(Maps.DIV));
    ops.add(new NullOp(Maps.MOD));
    ops.add(new NullOp(Maps.POWER));

    ops.add(new NullAndOp());
    ops.add(new NullOrOp());

    ops.add(new NullOp(Maps.STARTS_WITH));
    ops.add(new NullOp(Maps.ENDS_WITH));
    ops.add(new NullOp(Maps.CONTAINS));
    ops.add(new NullOp(Maps.LIKE));
    ops.add(new NullOp(Maps.TEXT_LEFT));
    ops.add(new NullOp(Maps.TEXT_RIGHT));

    ops.add(new CoalescingNullOp(Maps.MIN));
    ops.add(new CoalescingNullOp(Maps.MAX));

    return ops;
  }

  @Override
  public boolean isBinaryOpVectorized(String name) {
    return ops.isSupportedBinary(name);
  }

  @Override
  public Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemAggregator problemAggregator) {
    return ops.runBinaryMap(name, this, argument, problemAggregator);
  }

  @Override
  public Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemAggregator problemAggregator) {
    return ops.runZip(name, this, argument, problemAggregator);
  }

  @Override
  public boolean isTernaryOpVectorized(String name) {
    return ops.isSupportedTernary(name);
  }

  @Override
  public Storage<?> runVectorizedTernaryMap(
      String name,
      Object argument0,
      Object argument1,
      MapOperationProblemAggregator problemAggregator) {
    return ops.runTernaryMap(name, this, argument0, argument1, problemAggregator);
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
    return new NullStorage(limit - offset);
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

  /**
   * A binary operation that always returns the other argument.
   *
   * <p>Useful for implementing operations that should return the other argument when the left-hand
   * side is null, e.g. min.
   */
  private static class CoalescingNullOp extends BinaryMapOperation<Void, NullStorage> {
    public CoalescingNullOp(String name) {
      super(name);
    }

    @Override
    public Storage<?> runBinaryMap(
        NullStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      int checkedSize = Builder.checkSize(storage.getSize());
      return Storage.fromRepeatedItem(Value.asValue(arg), checkedSize, problemAggregator);
    }

    @Override
    public Storage<?> runZip(
        NullStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
      return arg;
    }
  }

  private abstract static class BoolAndNullOp extends BinaryMapOperation<Void, NullStorage> {
    public BoolAndNullOp(String name) {
      super(name);
    }

    protected abstract Boolean doBool(boolean a);

    @Override
    public Storage<?> runBinaryMap(
        NullStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        return new NullStorage(storage.getSize());
      } else if (arg instanceof Boolean b) {
        int checkedSize = Builder.checkSize(storage.getSize());
        return Storage.fromRepeatedItem(Value.asValue(doBool(b)), checkedSize, problemAggregator);
      } else {
        throw new UnexpectedTypeException("Boolean", arg.toString());
      }
    }

    @Override
    public Storage<?> runZip(
        NullStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
      BuilderForBoolean builder = Builder.getForBoolean(storage.getSize());
      for (long i = 0; i < storage.getSize(); i++) {
        if (arg.isNothing(i)) {
          builder.appendNulls(1);
        } else if (arg.getItemBoxed(i) instanceof Boolean bool) {
          builder.append(doBool(bool));
        } else {
          throw new UnexpectedTypeException("Boolean", arg.getItemBoxed(i).toString());
        }
      }
      return builder.seal();
    }
  }

  private static class NullAndOp extends BoolAndNullOp {
    public NullAndOp() {
      super(Maps.AND);
    }

    @Override
    protected Boolean doBool(boolean a) {
      if (a) {
        return null;
      } else {
        return false;
      }
    }
  }

  private static class NullOrOp extends BoolAndNullOp {
    public NullOrOp() {
      super(Maps.OR);
    }

    @Override
    protected Boolean doBool(boolean a) {
      if (a) {
        return true;
      } else {
        return null;
      }
    }
  }
}
