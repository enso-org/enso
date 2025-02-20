package org.enso.table.data.column.operation.map.text;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public abstract class StringBooleanOp
    extends BinaryMapOperation<String, SpecializedStorage<String>> {
  public StringBooleanOp(String name) {
    super(name);
  }

  protected abstract boolean doString(String a, String b);

  protected boolean doObject(String a, Object o) {
    throw new UnexpectedTypeException("a Text", o.toString());
  }

  @Override
  public Storage<Boolean> runBinaryMap(
      SpecializedStorage<String> storage,
      Object arg,
      MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return BoolStorage.makeEmpty(storage.getSize());
    } else if (arg instanceof String argString) {
      Context context = Context.getCurrent();
      long size = storage.getSize();
      var builder = Builder.getForBoolean(size);
      for (long i = 0; i < size; i++) {
        if (storage.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          builder.appendBoolean(doString(storage.getItemBoxed(i), argString));
        }
        context.safepoint();
      }
      return builder.seal();
    } else {
      Context context = Context.getCurrent();
      long size = storage.getSize();
      var builder = Builder.getForBoolean(size);
      for (long i = 0; i < size; i++) {
        if (storage.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          builder.appendBoolean(doObject(storage.getItemBoxed(i), arg));
        }
        context.safepoint();
      }
      return builder.seal();
    }
  }

  @Override
  public Storage<Boolean> runZip(
      SpecializedStorage<String> storage,
      Storage<?> arg,
      MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    if (arg instanceof StringStorage v) {
      long size = storage.getSize();
      var builder = Builder.getForBoolean(size);
      for (long i = 0; i < size; i++) {
        if (!storage.isNothing(i) && i < v.getSize() && !v.isNothing(i)) {
          builder.appendBoolean(doString(storage.getItemBoxed(i), v.getItemBoxed(i)));
        } else {
          builder.appendNulls(1);
        }
        context.safepoint();
      }
      return builder.seal();
    } else {
      long size = storage.getSize();
      var builder = Builder.getForBoolean(size);
      for (long i = 0; i < size; i++) {
        if (!storage.isNothing(i) && i < arg.getSize() && !arg.isNothing(i)) {
          Object x = arg.getItemBoxed(i);
          if (x instanceof String str) {
            builder.appendBoolean(doString(storage.getItemBoxed(i), str));
          } else {
            builder.appendBoolean(doObject(storage.getItemBoxed(i), x));
          }
        } else {
          builder.appendNulls(1);
        }
        context.safepoint();
      }
      return builder.seal();
    }
  }
}
