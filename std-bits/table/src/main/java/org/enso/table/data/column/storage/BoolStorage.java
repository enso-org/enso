package org.enso.table.data.column.storage;

import java.util.BitSet;
import java.util.List;
import java.util.function.IntFunction;
import org.enso.base.CompareException;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.bool.BooleanIsInOp;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.error.UnexpectedColumnTypeException;
import org.enso.table.error.UnexpectedTypeException;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.BitSets;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A boolean column storage. */
public final class BoolStorage extends Storage<Boolean>
    implements ColumnBooleanStorage, ColumnStorageWithNothingMap {
  private static final MapOperationStorage<Boolean, BoolStorage> ops = buildOps();
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

  public static BoolStorage makeEmpty(int size) {
    BitSet isNothing = new BitSet(size);
    isNothing.set(0, size);
    return new BoolStorage(new BitSet(), isNothing, size, false);
  }

  public static BoolStorage makeConstant(int size, boolean r) {
    return new BoolStorage(new BitSet(), new BitSet(), size, r);
  }

  public BoolStorage makeNegated() {
    return new BoolStorage(values, isNothing, size, !negated);
  }

  @Override
  public int size() {
    return size;
  }

  @Override
  public StorageType getType() {
    return BooleanType.INSTANCE;
  }

  @Override
  public Boolean getItemBoxed(int idx) {
    return isNothing.get(idx) ? null : getItem(idx);
  }

  public boolean getItem(long idx) {
    return negated != values.get((int) idx);
  }

  @Override
  public boolean isNothing(long idx) {
    return isNothing.get((int) idx);
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

  public BitSet getValues() {
    return values;
  }

  /**
   * Creates a new BoolStorage in which all missing values have been replaced by arg.
   *
   * <p>It works by setting the new isMissing to an empty bitset and changing the values bitset
   * accordingly. If `arg` is true, new values are `values || isMissing` and if `arg` is false, new
   * values are `values && (~isMissing)`.
   */
  private BoolStorage fillMissingBoolean(boolean arg) {
    final var newValues = (BitSet) values.clone();
    if (arg != negated) {
      newValues.or(isNothing);
    } else {
      newValues.andNot(isNothing);
    }
    return new BoolStorage(newValues, new BitSet(), size, negated);
  }

  @Override
  public Storage<?> fillMissing(
      Value arg, StorageType commonType, ProblemAggregator problemAggregator) {
    if (arg.isBoolean()) {
      return fillMissingBoolean(arg.asBoolean());
    } else {
      return super.fillMissing(arg, commonType, problemAggregator);
    }
  }

  @Override
  public Storage<?> fillMissingFromPrevious(BoolStorage missingIndicator) {
    if (missingIndicator != null) {
      throw new IllegalStateException(
          "Custom missing value semantics are not supported by BoolStorage.");
    }

    boolean previousValue = false;
    boolean hasPrevious = false;
    BitSet newIsNothing = new BitSet();
    BitSet newValues = new BitSet();

    Context context = Context.getCurrent();
    for (int i = 0; i < size; i++) {
      boolean isCurrentValueMissing = isNothing.get(i);
      if (isCurrentValueMissing) {
        if (hasPrevious) {
          newValues.set(i, previousValue);
        } else {
          newIsNothing.set(i);
        }
      } else {
        boolean currentValue = getItem(i);
        newValues.set(i, currentValue);
        previousValue = currentValue;
        hasPrevious = true;
      }

      context.safepoint();
    }

    return new BoolStorage(newValues, newIsNothing, size, false);
  }

  @Override
  public BoolStorage applyFilter(BitSet filterMask, int newLength) {
    Context context = Context.getCurrent();
    BitSet newIsNothing = new BitSet();
    BitSet newValues = new BitSet();
    int resultIx = 0;
    for (int i = 0; i < size; i++) {
      if (filterMask.get(i)) {
        if (isNothing.get(i)) {
          newIsNothing.set(resultIx++);
        } else if (values.get(i)) {
          newValues.set(resultIx++);
        } else {
          // We don't set any bits, but still increment the counter to indicate that we have just
          // 'inserted' a false value.
          resultIx++;
        }
      }

      context.safepoint();
    }
    return new BoolStorage(newValues, newIsNothing, newLength, negated);
  }

  @Override
  public BoolStorage applyMask(OrderMask mask) {
    Context context = Context.getCurrent();
    BitSet newNa = new BitSet();
    BitSet newVals = new BitSet();
    for (int i = 0; i < mask.length(); i++) {
      int position = mask.get(i);
      if (position == Storage.NOT_FOUND_INDEX || isNothing.get(position)) {
        newNa.set(i);
      } else if (values.get(position)) {
        newVals.set(i);
      }

      context.safepoint();
    }
    return new BoolStorage(newVals, newNa, mask.length(), negated);
  }

  public boolean isNegated() {
    return negated;
  }

  public Storage<?> iif(
      Value when_true,
      Value when_false,
      StorageType resultStorageType,
      ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    var on_true = makeRowProvider(when_true);
    var on_false = makeRowProvider(when_false);
    Builder builder = Builder.getForType(resultStorageType, size, problemAggregator);
    for (int i = 0; i < size; i++) {
      if (isNothing.get(i)) {
        builder.append(null);
      } else if (getItem(i)) {
        builder.append(on_true.apply(i));
      } else {
        builder.append(on_false.apply(i));
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private static IntFunction<Object> makeRowProvider(Value value) {
    if (value.isHostObject() && value.asHostObject() instanceof Storage<?> s) {
      return i -> (Object) s.getItemBoxed(i);
    }
    var converted = Polyglot_Utils.convertPolyglotValue(value);
    return i -> converted;
  }

  private static MapOperationStorage<Boolean, BoolStorage> buildOps() {
    MapOperationStorage<Boolean, BoolStorage> ops = new MapOperationStorage<>();
    ops.add(new BoolEq())
        .add(new BoolAnd())
        .add(new BoolOr())
        .add(new BooleanIsInOp())
        .add(new BoolLess())
        .add(new BoolLessOrEqual())
        .add(new BoolGreaterOrEqual())
        .add(new BoolGreater())
        .add(new BoolMin())
        .add(new BoolMax());
    return ops;
  }

  /** Creates a mask that selects elements corresponding to true entries in the passed storage. */
  public static BitSet toMask(BoolStorage storage) {
    BitSet mask = storage.normalize();
    mask.andNot(storage.getIsNothingMap());
    return mask;
  }

  /**
   * Returns a BitSet representation of the storage. It is the same as the values BitSet, but with
   * an assumption that the negated flag is false.
   */
  private BitSet normalize() {
    BitSet set = new BitSet();
    set.or(this.values);
    if (this.negated) {
      set.flip(0, this.size);
    }
    return set;
  }

  /** Acts like {@link #normalize} but also negates the bits. */
  private BitSet negateNormalize() {
    BitSet set = new BitSet();
    set.or(this.values);
    if (!this.negated) {
      set.flip(0, this.size);
    }
    return set;
  }

  @Override
  public BoolStorage slice(int offset, int limit) {
    int newSize = Math.min(size - offset, limit);
    return new BoolStorage(
        values.get(offset, offset + limit),
        isNothing.get(offset, offset + limit),
        newSize,
        negated);
  }

  @Override
  public Storage<?> appendNulls(int count) {
    BitSet isNothing = BitSets.makeDuplicate(this.isNothing);
    isNothing.set(size, size + count);
    return new BoolStorage(values, isNothing, size + count, negated);
  }

  @Override
  public BoolStorage slice(List<SliceRange> ranges) {
    Context context = Context.getCurrent();
    int newSize = SliceRange.totalLength(ranges);
    BitSet newValues = new BitSet(newSize);
    BitSet newIsNothing = new BitSet(newSize);
    int offset = 0;
    for (SliceRange range : ranges) {
      int length = range.end() - range.start();
      for (int i = 0; i < length; ++i) {
        newValues.set(offset + i, values.get(range.start() + i));
        newIsNothing.set(offset + i, isNothing.get(range.start() + i));
        context.safepoint();
      }
      offset += length;
    }

    return new BoolStorage(newValues, newIsNothing, newSize, negated);
  }

  @Override
  public BitSet getIsNothingMap() {
    return isNothing;
  }

  @Override
  public boolean get(long index) throws ValueIsNothingException {
    if (isNothing(index)) {
      throw new ValueIsNothingException(index);
    }
    return getItem(index);
  }

  private static class BoolEq extends BinaryMapOperation<Boolean, BoolStorage> {
    public BoolEq() {
      super(Maps.EQ);
    }

    @Override
    public BoolStorage runBinaryMap(
        BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        return BoolStorage.makeEmpty(storage.size);
      } else if (arg instanceof Boolean v) {
        if (v) {
          return storage;
        } else {
          return storage.makeNegated();
        }
      } else {
        return new BoolStorage(new BitSet(), storage.isNothing, storage.size, false);
      }
    }

    @Override
    public BoolStorage runZip(
        BoolStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
      Context context = Context.getCurrent();
      BitSet out = new BitSet();
      BitSet isNothing = new BitSet();
      for (int i = 0; i < storage.size; i++) {
        if (!storage.isNothing(i) && i < arg.size() && !arg.isNothing(i)) {
          if (((Boolean) storage.getItem(i)).equals(arg.getItemBoxed(i))) {
            out.set(i);
          }
        } else {
          isNothing.set(i);
        }

        context.safepoint();
      }
      return new BoolStorage(out, isNothing, storage.size, false);
    }
  }

  private static class BoolAnd extends BinaryMapOperation<Boolean, BoolStorage> {
    public BoolAnd() {
      super(Maps.AND);
    }

    @Override
    public BoolStorage runBinaryMap(
        BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        if (storage.negated) {
          var newMissing = new BitSet(storage.size);
          newMissing.flip(0, storage.size);
          newMissing.xor(storage.values);
          return new BoolStorage(storage.values, newMissing, storage.size, true);
        } else {
          var newMissing = storage.isNothing.get(0, storage.size);
          newMissing.or(storage.values);
          return new BoolStorage(new BitSet(), newMissing, storage.size, false);
        }
      } else if (arg instanceof Boolean v) {
        return v ? storage : new BoolStorage(new BitSet(), new BitSet(), storage.size, false);
      } else {
        throw new UnexpectedTypeException("a Boolean");
      }
    }

    @Override
    public BoolStorage runZip(
        BoolStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
      if (!(arg instanceof BoolStorage v)) {
        throw new UnexpectedColumnTypeException("Boolean");
      }

      BitSet out = v.values.get(0, storage.size);
      boolean negated;
      if (storage.negated && v.negated) {
        out.or(storage.values);
        negated = true;
      } else if (storage.negated) {
        out.andNot(storage.values);
        negated = false;
      } else if (v.negated) {
        out.flip(0, storage.size);
        out.and(storage.values);
        negated = false;
      } else {
        out.and(storage.values);
        negated = false;
      }

      BitSet isNothing = BitSets.makeDuplicate(storage.isNothing);
      isNothing.or(v.isNothing);
      if (storage.size > v.size) {
        isNothing.set(v.size, storage.size);
      }
      int current = isNothing.nextSetBit(0);
      while (current != -1) {
        Boolean a = storage.getItemBoxed(current);
        Boolean b = (current < v.size) ? v.getItemBoxed(current) : null;
        if (a == Boolean.FALSE || b == Boolean.FALSE) {
          isNothing.clear(current);
          boolean falseValue = negated;
          out.set(current, falseValue);
        }
        current = isNothing.nextSetBit(current + 1);
      }

      return new BoolStorage(out, isNothing, storage.size, negated);
    }
  }

  private static class BoolOr extends BinaryMapOperation<Boolean, BoolStorage> {
    public BoolOr() {
      super(Maps.OR);
    }

    @Override
    public BoolStorage runBinaryMap(
        BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        if (storage.negated) {
          var newMissing = storage.isNothing.get(0, storage.size);
          newMissing.or(storage.values);
          return new BoolStorage(new BitSet(), newMissing, storage.size, true);
        } else {
          var newMissing = new BitSet(storage.size);
          newMissing.flip(0, storage.size);
          newMissing.xor(storage.values);
          return new BoolStorage(storage.values, newMissing, storage.size, false);
        }
      } else if (arg instanceof Boolean v) {
        return v ? new BoolStorage(new BitSet(), new BitSet(), storage.size, true) : storage;
      } else {
        throw new UnexpectedTypeException("a Boolean");
      }
    }

    @Override
    public BoolStorage runZip(
        BoolStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
      if (!(arg instanceof BoolStorage v)) {
        throw new UnexpectedColumnTypeException("Boolean");
      }

      BitSet out = v.values.get(0, storage.size);
      boolean negated;
      if (storage.negated && v.negated) {
        out.and(storage.values);
        negated = true;
      } else if (storage.negated) {
        out.flip(0, storage.size);
        out.and(storage.values);
        negated = true;
      } else if (v.negated) {
        out.flip(0, storage.size);
        out.or(storage.values);
        negated = false;
      } else {
        out.or(storage.values);
        negated = false;
      }

      BitSet isNothing = BitSets.makeDuplicate(storage.isNothing);
      isNothing.or(v.isNothing);
      if (storage.size > v.size) {
        isNothing.set(v.size, storage.size);
      }
      int current = isNothing.nextSetBit(0);
      while (current != -1) {
        Boolean a = storage.getItemBoxed(current);
        Boolean b = (current < v.size) ? v.getItemBoxed(current) : null;
        if (a == Boolean.TRUE || b == Boolean.TRUE) {
          isNothing.clear(current);
          boolean trueValue = !negated;
          out.set(current, trueValue);
        }
        current = isNothing.nextSetBit(current + 1);
      }

      return new BoolStorage(out, isNothing, storage.size, negated);
    }
  }

  private abstract static class BoolCompareOp extends BinaryMapOperation<Boolean, BoolStorage> {
    public BoolCompareOp(String name) {
      super(name);
    }

    protected abstract boolean doCompare(boolean a, boolean b);

    @Override
    public Storage<?> runZip(
        BoolStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
      if (arg instanceof BoolStorage argBoolStorage) {
        BitSet out = new BitSet();
        BitSet isNothing = new BitSet();
        int n = storage.size;
        int m = Math.min(n, argBoolStorage.size);
        Context context = Context.getCurrent();
        for (int i = 0; i < m; i++) {
          if (storage.isNothing(i) || argBoolStorage.isNothing(i)) {
            isNothing.set(i);
          } else {
            boolean a = storage.getItem(i);
            boolean b = argBoolStorage.getItem(i);
            boolean r = doCompare(a, b);
            out.set(i, r);
          }

          context.safepoint();
        }

        isNothing.set(m, n);

        return new BoolStorage(out, isNothing, storage.size, false);
      } else if (arg.getType() instanceof AnyObjectType) {
        BitSet out = new BitSet();
        BitSet isNothing = new BitSet();
        int n = storage.size;
        int m = Math.min(n, arg.size());
        Context context = Context.getCurrent();
        for (int i = 0; i < m; i++) {
          if (storage.isNothing(i) || arg.isNothing(i)) {
            isNothing.set(i);
          } else {
            boolean a = storage.getItem(i);
            Object b = arg.getItemBoxed(i);
            if (b instanceof Boolean bBool) {
              boolean r = doCompare(a, bBool);
              out.set(i, r);
            } else {
              assert b != null;
              throw new CompareException(a, b);
            }
          }

          context.safepoint();
        }

        isNothing.set(m, n);

        return new BoolStorage(out, isNothing, storage.size, false);
      } else {
        throw new UnexpectedColumnTypeException("Boolean");
      }
    }
  }

  private static class BoolLess extends BoolCompareOp {
    public BoolLess() {
      super(Maps.LT);
    }

    @Override
    public Storage<?> runBinaryMap(
        BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        return BoolStorage.makeEmpty(storage.size);
      }

      if (arg instanceof Boolean b) {
        if (b) {
          // false is smaller than true, so we want to negate
          return new BoolStorage(storage.negateNormalize(), storage.isNothing, storage.size, false);
        } else {
          // nothing is strictly smaller than false
          return new BoolStorage(new BitSet(), storage.isNothing, storage.size, false);
        }
      } else {
        throw new UnexpectedTypeException("a Boolean", arg.toString());
      }
    }

    @Override
    protected boolean doCompare(boolean a, boolean b) {
      return !a && b;
    }
  }

  private static class BoolLessOrEqual extends BoolCompareOp {
    public BoolLessOrEqual() {
      super(Maps.LTE);
    }

    @Override
    public Storage<?> runBinaryMap(
        BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        return BoolStorage.makeEmpty(storage.size);
      }

      if (arg instanceof Boolean b) {
        if (b) {
          // everything is <= true
          return new BoolStorage(new BitSet(), storage.isNothing, storage.size, true);
        } else {
          // false is <= false
          return new BoolStorage(storage.negateNormalize(), storage.isNothing, storage.size, false);
        }
      } else {
        throw new UnexpectedTypeException("a Boolean", arg.toString());
      }
    }

    @Override
    protected boolean doCompare(boolean a, boolean b) {
      return !a || b;
    }
  }

  private static class BoolGreater extends BoolCompareOp {
    public BoolGreater() {
      super(Maps.GT);
    }

    @Override
    public Storage<?> runBinaryMap(
        BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        return BoolStorage.makeEmpty(storage.size);
      }

      if (arg instanceof Boolean b) {
        if (b) {
          // nothing is strictly greater than true
          return new BoolStorage(new BitSet(), storage.isNothing, storage.size, false);
        } else {
          // true is > false, so we just return as-is
          return storage;
        }
      } else {
        throw new UnexpectedTypeException("a Boolean", arg.toString());
      }
    }

    @Override
    protected boolean doCompare(boolean a, boolean b) {
      return a && !b;
    }
  }

  private static class BoolGreaterOrEqual extends BoolCompareOp {
    public BoolGreaterOrEqual() {
      super(Maps.GTE);
    }

    @Override
    public Storage<?> runBinaryMap(
        BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        return BoolStorage.makeEmpty(storage.size);
      }

      if (arg instanceof Boolean b) {
        if (b) {
          // true is >= true
          return storage;
        } else {
          // everything is >= false
          return new BoolStorage(new BitSet(), storage.isNothing, storage.size, true);
        }
      } else {
        throw new UnexpectedTypeException("a Boolean", arg.toString());
      }
    }

    @Override
    protected boolean doCompare(boolean a, boolean b) {
      return a || !b;
    }
  }

  private abstract static class BoolCoalescingOp extends BinaryMapOperation<Boolean, BoolStorage> {
    public BoolCoalescingOp(String name) {
      super(name);
    }

    protected abstract boolean doOperation(boolean a, boolean b);

    @Override
    public Storage<?> runZip(
        BoolStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
      if (arg instanceof BoolStorage argBoolStorage) {
        int n = storage.size;
        int m = Math.min(n, argBoolStorage.size());
        BitSet out = new BitSet();
        BitSet isNothing = new BitSet();
        Context context = Context.getCurrent();
        for (int i = 0; i < m; i++) {
          boolean isNothingA = storage.isNothing(i);
          boolean isNothingB = argBoolStorage.isNothing(i);
          if (isNothingA && isNothingB) {
            isNothing.set(i);
          } else {
            if (isNothingA) {
              out.set(i, argBoolStorage.getItem(i));
            } else if (isNothingB) {
              out.set(i, storage.getItem(i));
            } else {
              out.set(i, doOperation(storage.getItem(i), argBoolStorage.getItem(i)));
            }
          }

          context.safepoint();
        }

        for (int i = m; i < n; i++) {
          if (storage.isNothing(i)) {
            isNothing.set(i);
          } else {
            out.set(i, storage.getItem(i));
          }

          context.safepoint();
        }

        return new BoolStorage(out, isNothing, storage.size, false);
      } else {
        throw new UnexpectedColumnTypeException("Boolean");
      }
    }
  }

  private static class BoolMin extends BoolCoalescingOp {
    public BoolMin() {
      super(Maps.MIN);
    }

    @Override
    public BoolStorage runBinaryMap(
        BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        return storage;
      }

      if (arg instanceof Boolean b) {
        if (b) {
          // true is larger than false, so we want to keep values as is, and fill missing ones with
          // true
          return storage.fillMissingBoolean(true);
        } else {
          // false is smaller than everything:
          return BoolStorage.makeConstant(storage.size, false);
        }
      } else {
        throw new UnexpectedTypeException("a Boolean", arg.toString());
      }
    }

    @Override
    protected boolean doOperation(boolean a, boolean b) {
      return a && b;
    }
  }

  private static class BoolMax extends BoolCoalescingOp {
    public BoolMax() {
      super(Maps.MAX);
    }

    @Override
    public BoolStorage runBinaryMap(
        BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
      if (arg == null) {
        return storage;
      }

      if (arg instanceof Boolean b) {
        if (b) {
          // true is larger than everything:
          return BoolStorage.makeConstant(storage.size, true);
        } else {
          // false is smaller than true, so we only fill null gaps with it
          return storage.fillMissingBoolean(false);
        }
      } else {
        throw new UnexpectedTypeException("a Boolean", arg.toString());
      }
    }

    @Override
    protected boolean doOperation(boolean a, boolean b) {
      return a || b;
    }
  }
}
