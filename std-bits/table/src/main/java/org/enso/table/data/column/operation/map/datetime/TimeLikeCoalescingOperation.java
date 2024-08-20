package org.enso.table.data.column.operation.map.datetime;

import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public abstract class TimeLikeCoalescingOperation<T>
    extends BinaryMapOperation<T, SpecializedStorage<T>> {
  private final Class<T> inputTypeClass;

  public TimeLikeCoalescingOperation(String name, Class<T> inputTypeClass) {
    super(name);
    this.inputTypeClass = inputTypeClass;
  }

  protected abstract Builder createOutputBuilder(int size);

  protected abstract T doOperation(T a, T b);

  @Override
  public Storage<?> runBinaryMap(
      SpecializedStorage<T> storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    int size = storage.size();
    if (arg == null) {
      return storage;
    } else {
      Object adapted = Polyglot_Utils.convertPolyglotValue(arg);
      if (inputTypeClass.isInstance(adapted)) {
        T casted = inputTypeClass.cast(adapted);
        int n = storage.size();
        Builder builder = createOutputBuilder(n);
        Context context = Context.getCurrent();
        for (int i = 0; i < size; i++) {
          T r = storage.isNothing(i) ? casted : doOperation(storage.getItemBoxed(i), casted);
          builder.appendNoGrow(r);
          context.safepoint();
        }

        return builder.seal();
      } else {
        throw new UnexpectedTypeException(
            "a " + inputTypeClass.getName() + " but got " + arg.getClass().getName());
      }
    }
  }

  @Override
  public Storage<?> runZip(
      SpecializedStorage<T> storage,
      Storage<?> arg,
      MapOperationProblemAggregator problemAggregator) {
    if (arg.getType().equals(storage.getType())) {
      if (arg instanceof SpecializedStorage<?> argStorage) {
        SpecializedStorage<T> argTStorage = storage.castIfSameType(argStorage);
        int n = storage.size();
        Builder builder = createOutputBuilder(n);
        Context context = Context.getCurrent();
        for (int i = 0; i < n; i++) {
          T a = storage.getItemBoxed(i);
          T b = argTStorage.getItemBoxed(i);
          T r;
          if (a == null && b == null) {
            r = null;
          } else {
            if (a == null) {
              r = b;
            } else if (b == null) {
              r = a;
            } else {
              r = doOperation(a, b);
            }
          }

          builder.appendNoGrow(r);
          context.safepoint();
        }

        return builder.seal();
      } else {
        throw new IllegalArgumentException(
            "Unexpected storage implementation for type "
                + storage.getType()
                + ": "
                + arg.getClass().getCanonicalName());
      }
    } else {
      throw new UnexpectedTypeException("a " + storage.getType() + " column");
    }
  }
}
