package org.enso.table.data.column.operation.map.bool;

import java.util.List;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.util.ImmutableBitSet;
import org.graalvm.polyglot.Context;

/**
 * A specialized implementation for the IS_IN operation on booleans - since booleans have just three
 * possible values we can have a highly efficient implementation that does not even rely on hashmap
 * and after processing the input vector, performs the checks using only BitSet builtins.
 */
public class BooleanIsInOp extends BinaryMapOperation<Boolean, BoolStorage> {
  public BooleanIsInOp() {
    super(Storage.Maps.IS_IN);
  }

  @Override
  public BoolStorage runBinaryMap(
      BoolStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg instanceof List) {
      return runMap(storage, (List<?>) arg);
    } else {
      throw new IllegalArgumentException("Argument to `is_in` must be a vector.");
    }
  }

  public BoolStorage runMap(BoolStorage storage, List<?> arg) {
    boolean hadNull = false;
    boolean hadTrue = false;
    boolean hadFalse = false;

    Context context = Context.getCurrent();
    for (Object o : arg) {
      switch (o) {
        case Boolean b -> {
          hadTrue |= b;
          hadFalse |= !b;
        }
        case null -> hadNull = true;
        default -> {}
      }

      context.safepoint();
    }

    return run(storage, hadNull, hadTrue, hadFalse);
  }

  @Override
  public Storage<?> runZip(
      BoolStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    // We could try BitSets for BoolStorage, but it is unclear if they will improve performance due
    // to need for additional allocations. It does not seem worth optimizing this rare usecase
    // currently.
    return runMap(storage, arg.toList());
  }

  private BoolStorage run(BoolStorage storage, boolean hadNull, boolean hadTrue, boolean hadFalse) {
    int size = storage.size();
    ImmutableBitSet values = new ImmutableBitSet(storage.getValues(), size);
    ImmutableBitSet isNothing = new ImmutableBitSet(storage.getIsNothingMap(), size);
    boolean negated = storage.isNegated();

    ImmutableBitSet newValues;
    ImmutableBitSet newIsNothing;

    if (hadTrue && !hadFalse) {
      newValues = storage.isNegated() ? isNothing.notAndNot(values) : isNothing.notAnd(values);
      newIsNothing =
          hadNull
              ? (storage.isNegated() ? isNothing.or(values) : isNothing.orNot(values))
              : isNothing;
    } else if (!hadTrue && hadFalse) {
      newValues = storage.isNegated() ? isNothing.notAnd(values) : isNothing.notAndNot(values);
      newIsNothing =
          hadNull
              ? (storage.isNegated() ? isNothing.orNot(values) : isNothing.or(values))
              : isNothing;
    } else if (hadTrue) {
      newValues = isNothing.not();
      newIsNothing = isNothing;
    } else {
      newValues = ImmutableBitSet.allFalse(size);
      newIsNothing = hadNull ? ImmutableBitSet.allTrue(size) : ImmutableBitSet.allFalse(size);
    }

    return new BoolStorage(newValues.toBitSet(), newIsNothing.toBitSet(), size, false);
  }
}
