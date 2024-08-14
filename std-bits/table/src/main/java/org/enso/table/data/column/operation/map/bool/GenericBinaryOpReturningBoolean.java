package org.enso.table.data.column.operation.map.bool;

import java.util.BitSet;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.graalvm.polyglot.Context;

/**
 * A generic binary operation that takes two values of some type T and returns a boolean.
 *
 * <p>If any of the two values is null, the result will also be null.
 */
public abstract class GenericBinaryOpReturningBoolean<T, S extends SpecializedStorage<T>>
    extends BinaryMapOperation<T, S> {

  public GenericBinaryOpReturningBoolean(String name) {
    super(name);
  }

  /**
   * Tries to cast an object to the type T.
   *
   * <p>Returns {@code null} if the types are not compatible.
   */
  protected abstract T tryCast(Object object);

  protected abstract boolean doOperation(T a, T b);

  protected abstract boolean doOther(T a, Object b);

  @Override
  public Storage<?> runBinaryMap(
      S storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return BoolStorage.makeEmpty(storage.size());
    } else {
      T argT = tryCast(arg);
      if (argT != null) {
        return runHomogenousMap(storage, argT);
      } else {
        return runMixedMap(storage, arg);
      }
    }
  }

  @Override
  public Storage<?> runZip(
      S storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    assert arg != null;
    if (storage.getType().equals(arg.getType())
        && arg instanceof SpecializedStorage<?> argStorage) {
      SpecializedStorage<T> argTStorage = storage.castIfSameType(argStorage);
      assert argTStorage != null : "We checked that types are equal so cast should not fail";
      return runHomogenousZip(storage, argTStorage);
    } else {
      return runMixedZip(storage, arg);
    }
  }

  private BoolStorage runHomogenousMap(S storage, T arg) {
    BitSet newVals = new BitSet();
    BitSet newIsNothing = new BitSet();
    Context context = Context.getCurrent();
    int n = storage.size();
    for (int i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        newIsNothing.set(i);
      } else {
        T storageItem = storage.getItemBoxed(i);
        assert storageItem != null : "isNothing returned true but element was null";
        boolean r = doOperation(storageItem, arg);
        newVals.set(i, r);
      }

      context.safepoint();
    }
    return new BoolStorage(newVals, newIsNothing, n, false);
  }

  private BoolStorage runMixedMap(S storage, Object arg) {
    BitSet newVals = new BitSet();
    BitSet newIsNothing = new BitSet();
    Context context = Context.getCurrent();
    int n = storage.size();
    for (int i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        newIsNothing.set(i);
      } else {
        T storageItem = storage.getItemBoxed(i);
        assert storageItem != null : "isNothing returned true but element was null";
        boolean r = doOther(storageItem, arg);
        newVals.set(i, r);
      }

      context.safepoint();
    }
    return new BoolStorage(newVals, newIsNothing, n, false);
  }

  private BoolStorage runHomogenousZip(S storage, SpecializedStorage<T> argStorage) {
    BitSet newVals = new BitSet();
    BitSet newIsNothing = new BitSet();
    Context context = Context.getCurrent();
    int n = storage.size();
    int m = argStorage.size();
    for (int i = 0; i < n; i++) {
      if (storage.isNothing(i) || !(i < m) || argStorage.isNothing(i)) {
        newIsNothing.set(i);
      } else {
        T storageItem = storage.getItemBoxed(i);
        T argItem = argStorage.getItemBoxed(i);
        assert storageItem != null : "isNothing returned true but element was null";
        assert argItem != null : "isNothing returned true but element was null";
        boolean r = doOperation(storageItem, argItem);
        newVals.set(i, r);
      }

      context.safepoint();
    }

    return new BoolStorage(newVals, newIsNothing, n, false);
  }

  private BoolStorage runMixedZip(S storage, Storage<?> argStorage) {
    BitSet newVals = new BitSet();
    BitSet newIsNothing = new BitSet();
    Context context = Context.getCurrent();
    int n = storage.size();
    int m = argStorage.size();
    for (int i = 0; i < n; i++) {
      if (storage.isNothing(i) || !(i < m) || argStorage.isNothing(i)) {
        newIsNothing.set(i);
      } else {
        T storageItem = storage.getItemBoxed(i);
        Object argItem = argStorage.getItemBoxed(i);
        assert storageItem != null : "isNothing returned true but element was null";
        assert argItem != null : "isNothing returned true but element was null";

        T argT = tryCast(argItem);
        boolean r = (argT != null) ? doOperation(storageItem, argT) : doOther(storageItem, argItem);
        newVals.set(i, r);
      }

      context.safepoint();
    }

    return new BoolStorage(newVals, newIsNothing, n, false);
  }
}
