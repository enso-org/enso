package org.enso.table.data.column.operation.map.text;

import java.util.BitSet;

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
          builder.appendBoolean(doString(storage.getBoxed(i), argString));
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
          builder.appendBoolean(doObject(storage.getBoxed(i), arg));
        }
        context.safepoint();
      }
      return builder.seal();
    }
  }

  @Override
  public BoolStorage runZip(
      SpecializedStorage<String> storage,
      Storage<?> arg,
      MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    if (arg instanceof StringStorage v) {
      BitSet newVals = new BitSet();
      BitSet newIsNothing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNothing(i) && i < v.size() && !v.isNothing(i)) {
          if (doString(storage.getBoxed(i), v.getBoxed(i))) {
            newVals.set(i);
          }
        } else {
          newIsNothing.set(i);
        }

        context.safepoint();
      }
      return new BoolStorage(newVals, newIsNothing, storage.size(), false);
    } else {
      BitSet newVals = new BitSet();
      BitSet newIsNothing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNothing(i) && i < arg.size() && !arg.isNothing(i)) {
          Object x = arg.getBoxed(i);
          if (x instanceof String str) {
            if (doString(storage.getBoxed(i), str)) {
              newVals.set(i);
            }
          } else {
            if (doObject(storage.getBoxed(i), x)) {
              newVals.set(i);
            }
          }
        } else {
          newIsNothing.set(i);
        }

        context.safepoint();
      }
      return new BoolStorage(newVals, newIsNothing, storage.size(), false);
    }
  }
}
