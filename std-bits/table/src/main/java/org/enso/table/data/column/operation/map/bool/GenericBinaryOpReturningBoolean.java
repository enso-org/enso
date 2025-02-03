package org.enso.table.data.column.operation.map.bool;

import org.enso.table.data.column.builder.Builder;
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
      return BoolStorage.makeEmpty(storage.getSize());
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

  private Storage<Boolean> runHomogenousMap(S storage, T arg) {
    Context context = Context.getCurrent();
    long n = storage.getSize();
    var builder = Builder.getForBoolean(n);
    for (long i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        T storageItem = storage.getItemBoxed(i);
        assert storageItem != null : "isNothing returned true but element was null";
        boolean r = doOperation(storageItem, arg);
        builder.append(r);
      }

      context.safepoint();
    }
    return builder.seal();
  }

  private Storage<Boolean> runMixedMap(S storage, Object arg) {
    Context context = Context.getCurrent();
    long n = storage.getSize();
    var builder = Builder.getForBoolean(n);
    for (long i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        T storageItem = storage.getItemBoxed(i);
        assert storageItem != null : "isNothing returned true but element was null";
        boolean r = doOther(storageItem, arg);
        builder.append(r);
      }

      context.safepoint();
    }
    return builder.seal();
  }

  private Storage<Boolean> runHomogenousZip(S storage, SpecializedStorage<T> argStorage) {
    long n = storage.getSize();
    long m = argStorage.getSize();

    var builder = Builder.getForBoolean(n);
    Context context = Context.getCurrent();
    for (long i = 0; i < n; i++) {
      if (i >= m || storage.isNothing(i) || argStorage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        T storageItem = storage.getItemBoxed(i);
        T argItem = argStorage.getItemBoxed(i);
        assert storageItem != null : "isNothing returned true but element was null";
        assert argItem != null : "isNothing returned true but element was null";
        boolean r = doOperation(storageItem, argItem);
        builder.appendBoolean(r);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private Storage<Boolean> runMixedZip(S storage, Storage<?> argStorage) {
    Context context = Context.getCurrent();
    long n = storage.getSize();
    long m = argStorage.getSize();
    var builder = Builder.getForBoolean(n);
    for (long i = 0; i < n; i++) {
      if (i >= m || storage.isNothing(i) || argStorage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        T storageItem = storage.getItemBoxed(i);
        Object argItem = argStorage.getItemBoxed(i);
        assert storageItem != null : "isNothing returned true but element was null";
        assert argItem != null : "isNothing returned true but element was null";

        T argT = tryCast(argItem);
        boolean r = (argT != null) ? doOperation(storageItem, argT) : doOther(storageItem, argItem);
        builder.append(r);
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
