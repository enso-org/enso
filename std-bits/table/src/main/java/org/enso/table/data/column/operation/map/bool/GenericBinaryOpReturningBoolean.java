package org.enso.table.data.column.operation.map.bool;

import java.util.BitSet;
import org.enso.table.data.column.builder.StorageTypeMismatchException;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

/**
 * A generic binary operation that takes two values of some type T and returns a boolean.
 *
 * <p>If any of the two values is null, the result will also be null.
 */
public abstract class GenericBinaryOpReturningBoolean<T, S extends SpecializedStorage<T>>
    extends BinaryMapOperation<T, S> {

  private final Class<T> myObjectType;

  public GenericBinaryOpReturningBoolean(String name, Class<T> objectType) {
    super(name);
    this.myObjectType = objectType;
  }

  protected abstract boolean doOperation(T a, T b);

  @Override
  public Storage<?> runBinaryMap(
      S storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return BoolStorage.makeEmpty(storage.size());
    } else if (myObjectType.isInstance(arg)) {
      T argT = myObjectType.cast(arg);
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
          boolean r = doOperation(storageItem, argT);
          newVals.set(i, r);
        }

        context.safepoint();
      }
      return new BoolStorage(newVals, newIsNothing, n, false);
    } else {
      throw new UnexpectedTypeException(myObjectType.getName());
    }
  }

  @Override
  public Storage<?> runZip(
      S storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    assert arg != null;
    if (!storage.getType().equals(arg.getType())) {
      throw new StorageTypeMismatchException(storage.getType(), arg.getType());
    }

    if (arg instanceof SpecializedStorage<?> argStorage) {
      SpecializedStorage<T> argTStorage = storage.castIfSameType(argStorage);
      assert argTStorage != null : "We checked that types are equal so cast should not fail";
      BitSet newVals = new BitSet();
      BitSet newIsNothing = new BitSet();
      Context context = Context.getCurrent();
      int n = storage.size();
      int m = argTStorage.size();
      for (int i = 0; i < n; i++) {
        if (storage.isNothing(i) || !(i < m) || argTStorage.isNothing(i)) {
          newIsNothing.set(i);
        } else {
          T storageItem = storage.getItemBoxed(i);
          T argItem = argTStorage.getItemBoxed(i);
          assert storageItem != null : "isNothing returned true but element was null";
          assert argItem != null : "isNothing returned true but element was null";
          boolean r = doOperation(storageItem, argItem);
          newVals.set(i, r);
        }

        context.safepoint();
      }

      return new BoolStorage(newVals, newIsNothing, n, false);
    } else {
      throw new IllegalStateException("Unexpected storage type: " + arg.getClass().getName());
    }
  }
}
