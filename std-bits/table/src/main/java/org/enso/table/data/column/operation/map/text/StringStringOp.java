package org.enso.table.data.column.operation.map.text;

import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public abstract class StringStringOp extends MapOperation<String, SpecializedStorage<String>> {
  public StringStringOp(String name) {
    super(name);
  }

  protected abstract String doString(String a, String b);

  @Override
  public Storage<?> runMap(SpecializedStorage<String> storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    int size = storage.size();
    if (arg == null) {
      StringBuilder builder = new StringBuilder(size);
      builder.appendNulls(size);
      return builder.seal();
    } else if (arg instanceof String argString) {
      String[] newVals = new String[size];
      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        if (storage.isNa(i)) {
          newVals[i] = null;
        } else {
          newVals[i] = doString(storage.getItem(i), argString);
        }

        context.safepoint();
      }
      return new StringStorage(newVals, size);
    } else {
      throw new UnexpectedTypeException("a Text");
    }
  }

  @Override
  public Storage<?> runZip(SpecializedStorage<String> storage, Storage<?> arg,
                           MapOperationProblemBuilder problemBuilder) {
    if (arg instanceof StringStorage v) {
      int size = storage.size();
      String[] newVals = new String[size];
      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        if (storage.isNa(i) || v.isNa(i)) {
          newVals[i] = null;
        } else {
          newVals[i] = doString(storage.getItem(i), v.getItem(i));
        }

        context.safepoint();
      }
      return new StringStorage(newVals, size);
    } else {
      throw new UnexpectedTypeException("a Text column");
    }
  }
}
