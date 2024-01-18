package org.enso.table.data.column.operation.map.bool;

import java.util.BitSet;
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
 * and after processing the input vector, performs the checks in constant time.
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
    ImmutableBitSet missing = new ImmutableBitSet(storage.getIsMissing(), size);
    boolean negated = storage.isNegated();

    ImmutableBitSet newValues;
    ImmutableBitSet newMissing;

    if (hadTrue && !hadFalse) {
      newValues = storage.isNegated() ? missing.notAndNot(values) : missing.notAnd(values);
      newMissing = hadNull ? (storage.isNegated() ? missing.or(values) : missing.orNot(values)) : missing;
    } else if (!hadTrue && hadFalse) {
      newValues = storage.isNegated() ? missing.notAnd(values) : missing.notAndNot(values);
      newMissing = hadNull ? (storage.isNegated() ? missing.orNot(values) : missing.or(values)) : missing;
    } else if (hadTrue && hadFalse) {
      newValues = missing.not();
      newMissing = missing;
    } else {
      newValues = ImmutableBitSet.allFalse(size);
      newMissing = hadNull ? ImmutableBitSet.allTrue(size) : ImmutableBitSet.allFalse(size);
    }

    return new BoolStorage(newValues.toBitSet(), newMissing.toBitSet(), size, false);

    /*
    BitSet values = storage.getValues();
    BitSet missing = storage.getIsMissing();
    boolean negated = storage.isNegated();

    BitSet newValues;
    BitSet newMissing;

    if (hadNull) {
      if (hadTrue) {
        if (hadFalse) {
          // hadNull=t hadTrue=t hadFalse=t
        } else {
          // hadNull=t hadTrue=t hadFalse=f
          if (negated) {
          } else {
          }
        }
      } else {
        if (hadFalse) {
          // hadNull=t hadTrue=f hadFalse=t
          if (negated) {
          } else {
          }
        } else {
          // hadNull=t hadTrue=f hadFalse=f
        }
      }
    } else {
      if (hadTrue) {
        if (hadFalse) {
          // hadNull=f hadTrue=t hadFalse=t
        } else {
          // hadNull=f hadTrue=t hadFalse=f
          if (negated) {
          } else {
          }
        }
      } else {
        if (hadFalse) {
          // hadNull=f hadTrue=f hadFalse=t
          newValues = (BitSet) missing.clone();
          newValues.flip(0, storage.size());
          if (negated) {
            newValues.and(values);
          } else {
            newValues.andNot(values);
          }
          newMissing = missing;
        } else {
          // hadNull=f hadTrue=f hadFalse=f
          // NULL.is_in([]) is false, not NULL
          newValues = new BitSet();
          newMissing = new BitSet();
        }
      }
    }

    return new BoolStorage(newValues, newMissing, storage.size(), false);
*/

    /*
    BitSet newVals = new BitSet(storage.size());
    BitSet newMissing = new BitSet(storage.size());
    byte[] bits = storage.getValues().toByteArray();
    System.out.println("vals " + bits);
    for (int i = 0; i < storage.size(); ++i) {
      boolean bitUnNegated = storage.getValues().get(i);
      boolean bit = storage.isNegated() ? !bitUnNegated : bitUnNegated;
      System.out.println("bits " + bitUnNegated + " " + bit);
      boolean inputValue = storage.getItem(i);
      boolean inputIsNull = storage.isNa(i);
      if (inputIsNull) {
        newMissing.set(i, true);
      } else if (inputValue) {
        if (hadTrue) {
          newVals.set(i, true);
        } else {
          if (hadNull) {
              newMissing.set(i, true);
          } else {
              newVals.set(i, false);
          }
        }
      } else { // inputValue==false
        if (hadFalse) {
          newVals.set(i, true);
        } else {
          if (hadNull) {
            newMissing.set(i, true);
          } else {
            newVals.set(i, false);
          }
        }
      }
    }

    return new BoolStorage(newVals, newMissing, storage.size(), false);
    */

/*
    boolean negated = false;

    if (hadNull && hadTrue && hadFalse) {
      // We use empty newVals which has everything set to false and negate it to make all of that
      // set to true with zero cost.
      newVals = new BitSet();
      negated = true;
    } else if (!hadNull && !hadTrue && !hadFalse) {
      // No values are present, so the result is to be false everywhere.
      newVals = new BitSet();
    } else if (hadNull && !hadTrue && !hadFalse) {
      // Only missing values are in the set, so we just return the missing indicator.
      newVals = storage.getIsMissing();
    } else if (hadTrue && hadFalse) { // && !hadNull
      // All non-missing values are in the set - so we just return the negated missing indicator.
      newVals = storage.getIsMissing();
      negated = true;
    } else {
      // hadTrue != hadFalse
      newVals = storage.getValues().get(0, storage.size());
      if (hadTrue) {
        if (storage.isNegated()) {
          newVals.flip(0, storage.size());
        }
      } else { // hadFalse
        if (!storage.isNegated()) {
          newVals.flip(0, storage.size());
        }
      }
      newVals.andNot(storage.getIsMissing());

      if (hadNull) {
        newVals.or(storage.getIsMissing());
      }
    }

    return new BoolStorage(newVals, new BitSet(), storage.size(), negated);
    */
  }
}
