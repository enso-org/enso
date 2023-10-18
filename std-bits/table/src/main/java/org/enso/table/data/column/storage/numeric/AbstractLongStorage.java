package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.operation.map.numeric.LongRoundOp;
import org.enso.table.data.column.operation.map.numeric.UnaryLongToLongOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.AddOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.DivideOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.ModOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MulOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.PowerOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.SubOp;
import org.enso.table.data.column.operation.map.numeric.comparisons.EqualsComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.GreaterComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.GreaterOrEqualComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.LessComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.LessOrEqualComparison;
import org.enso.table.data.column.operation.map.numeric.isin.LongIsInOp;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.graalvm.polyglot.Context;

public abstract class AbstractLongStorage extends NumericStorage<Long> {
  public abstract long getItem(int idx);

  public abstract BitSet getIsMissing();

  private static final MapOperationStorage<Long, AbstractLongStorage> ops = buildOps();

  @Override
  public boolean isUnaryOpVectorized(String name) {
    return ops.isSupportedUnary(name);
  }

  @Override
  public Storage<?> runVectorizedUnaryMap(
      String name, MapOperationProblemAggregator problemAggregator) {
    return ops.runUnaryMap(name, this, problemAggregator);
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
  public boolean isTernaryOpVectorized(String op) {
    return ops.isSupportedTernary(op);
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
  public Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemAggregator problemAggregator) {
    return ops.runZip(name, this, argument, problemAggregator);
  }

  @Override
  public abstract IntegerType getType();

  @Override
  public StorageType inferPreciseType() {
    return getType();
  }

  @Override
  public StorageType inferPreciseTypeShrunk() {
    // If the type is already smallest possible, we return it unchanged (we will return 8-bit
    // columns as-is, although
    // we will not shrink 16-bit columns to 8-bits even if it were possible).
    if (getType().bits().toInteger() <= 16) {
      return getType();
    }

    IntegerType[] possibleTypes =
        new IntegerType[] {IntegerType.INT_16, IntegerType.INT_32, IntegerType.INT_64};

    int currentTypeIdx = 0;
    int n = size();
    Context context = Context.getCurrent();
    for (int i = 0; i < n; i++) {
      if (isNa(i)) {
        continue;
      }

      long item = getItem(i);
      while (!possibleTypes[currentTypeIdx].fits(item)) {
        currentTypeIdx++;
      }

      if (currentTypeIdx >= possibleTypes.length - 1) {
        break;
      }

      context.safepoint();
    }

    return possibleTypes[currentTypeIdx];
  }

  private static MapOperationStorage<Long, AbstractLongStorage> buildOps() {
    MapOperationStorage<Long, AbstractLongStorage> ops = new MapOperationStorage<>();
    ops.add(new AddOp<>())
        .add(new SubOp<>())
        .add(new MulOp<>())
        .add(new DivideOp<>())
        .add(new ModOp<>())
        .add(new PowerOp<>())
        .add(
            new UnaryLongToLongOp(Maps.TRUNCATE) {
              @Override
              protected long doOperation(long a) {
                return a;
              }
            })
        .add(
            new UnaryLongToLongOp(Maps.CEIL) {
              @Override
              protected long doOperation(long a) {
                return a;
              }
            })
        .add(
            new UnaryLongToLongOp(Maps.FLOOR) {
              @Override
              protected long doOperation(long a) {
                return a;
              }
            })
        .add(new LongRoundOp(Maps.ROUND))
        .add(new LessComparison<>())
        .add(new LessOrEqualComparison<>())
        .add(new EqualsComparison<>())
        .add(new GreaterOrEqualComparison<>())
        .add(new GreaterComparison<>())
        .add(
            new UnaryMapOperation<>(Storage.Maps.IS_NOTHING) {
              @Override
              public BoolStorage runUnaryMap(
                  AbstractLongStorage storage, MapOperationProblemAggregator problemAggregator) {
                return new BoolStorage(storage.getIsMissing(), new BitSet(), storage.size(), false);
              }
            })
        .add(
            new UnaryMapOperation<>(Storage.Maps.IS_NAN) {
              @Override
              public BoolStorage runUnaryMap(
                  AbstractLongStorage storage, MapOperationProblemAggregator problemAggregator) {
                BitSet isNaN = new BitSet();
                return new BoolStorage(isNaN, storage.getIsMissing(), storage.size(), false);
              }
            })
        .add(
            new UnaryMapOperation<>(Storage.Maps.IS_INFINITE) {
              @Override
              public BoolStorage runUnaryMap(
                  AbstractLongStorage storage, MapOperationProblemAggregator problemAggregator) {
                BitSet isInfinite = new BitSet();
                return new BoolStorage(isInfinite, storage.getIsMissing(), storage.size(), false);
              }
            })
        .add(new LongIsInOp());
    return ops;
  }

  @Override
  public AbstractLongStorage fillMissingFromPrevious(BoolStorage missingIndicator) {
    if (missingIndicator != null) {
      throw new IllegalStateException(
          "Custom missing value semantics are not supported by AbstractLongStorage.");
    }

    int n = size();
    long[] newData = new long[n];
    BitSet newMissing = new BitSet();
    long previousValue = 0;
    boolean hasPrevious = false;

    Context context = Context.getCurrent();
    for (int i = 0; i < n; i++) {
      boolean isCurrentMissing = isNa(i);
      if (isCurrentMissing) {
        if (hasPrevious) {
          newData[i] = previousValue;
        } else {
          newMissing.set(i);
        }
      } else {
        long currentValue = getItem(i);
        newData[i] = currentValue;
        previousValue = currentValue;
        hasPrevious = true;
      }

      context.safepoint();
    }

    return new LongStorage(newData, n, newMissing, getType());
  }

  /**
   * Return an instance of storage containing the same data but with a wider type.
   *
   * <p>Ideally it should avoid copying the data, if it's possible.
   */
  public abstract AbstractLongStorage widen(IntegerType widerType);
}
