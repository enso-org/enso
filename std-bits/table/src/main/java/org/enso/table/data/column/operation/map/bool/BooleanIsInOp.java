package org.enso.table.data.column.operation.map.bool;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
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
    BitSet values = storage.getValues();
    BitSet missing = storage.getIsMissing();
    boolean negated = storage.isNegated();

    BitSet newValues;
    BitSet newMissing;

    if (hadNull) {
      if (hadTrue) {
        if (hadFalse) {
          // t t t
          newValues = (BitSet) missing.clone();
          newValues.flip(0, storage.size());
          newMissing = missing;
        } else {
          // t t f
          BitSet oldValuesFlipped = (BitSet) values.clone();
          oldValuesFlipped.flip(0, storage.size());
          if (negated) {
            newValues = (BitSet) missing.clone();
            newValues.flip(0, storage.size());
            newValues.and(oldValuesFlipped);
            newMissing = (BitSet) missing.clone();
            newMissing.or(values);
          } else {
            newValues = (BitSet) missing.clone();
            newValues.flip(0, storage.size());
            newValues.and(values);
            newMissing = (BitSet) missing.clone();
            newMissing.or(oldValuesFlipped);
          }
        }
      } else {
        if (hadFalse) {
          // t f t
          BitSet oldMissingFlipped = (BitSet) missing.clone();
          oldMissingFlipped.flip(0, storage.size());
          BitSet oldValuesFlipped = (BitSet) values.clone();
          oldValuesFlipped.flip(0, storage.size());
          if (negated) {
            newValues = oldMissingFlipped;
            newValues.and(values);
            newMissing = (BitSet) missing.clone();
            newMissing.or(oldValuesFlipped);
          } else {
            newValues = oldMissingFlipped;
            newValues.and(oldValuesFlipped);
            newMissing = (BitSet) missing.clone();
            newMissing.or(values);
          }
        } else {
          // t f f
          newValues = new BitSet(storage.size()); // Values don't matter
          newMissing = new BitSet(storage.size());
          newMissing.flip(0, storage.size());
        }
      }
    } else {
      if (hadTrue) {
        if (hadFalse) {
          // f t t
          newValues = (BitSet) missing.clone();
          newValues.flip(0, storage.size());
          newMissing = (BitSet) missing.clone();
        } else {
          // f t f
          newValues = (BitSet) missing.clone();
          newValues.flip(0, storage.size());
          if (negated) {
            BitSet oldValuesFlipped = (BitSet) values.clone();
            oldValuesFlipped.flip(0, storage.size());
            newValues.and(oldValuesFlipped);
          } else {
            newValues.and(values);
          }
          newMissing = missing;
        }
      } else {
        if (hadFalse) {
          // f f t
          newValues = (BitSet) missing.clone();
          newValues.flip(0, storage.size());
          if (negated) {
            newValues.and(values);
          } else {
            BitSet oldValuesFlipped = (BitSet) values.clone();
            oldValuesFlipped.flip(0, storage.size());
            newValues.and(oldValuesFlipped);
          }
          newMissing = missing;
        } else {
          // f f f
          newValues = missing;
          newMissing = missing;
        }
      }
    }

    return new BoolStorage(newValues, newMissing, storage.size(), false);

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
