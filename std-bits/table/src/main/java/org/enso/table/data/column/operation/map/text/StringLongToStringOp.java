package org.enso.table.data.column.operation.map.text;

import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public abstract class StringLongToStringOp
    extends BinaryMapOperation<String, SpecializedStorage<String>> {
  public StringLongToStringOp(String name) {
    super(name);
  }

  protected abstract String doOperation(String a, long b);

  @Override
  public Storage<?> runBinaryMap(
      SpecializedStorage<String> storage,
      Object arg,
      MapOperationProblemAggregator problemAggregator) {
    int size = storage.size();
    if (arg == null) {
      StringBuilder builder = new StringBuilder(size, TextType.VARIABLE_LENGTH);
      builder.appendNulls(size);
      return builder.seal();
    } else if (arg instanceof Long argLong) {
      String[] newVals = new String[size];
      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        if (storage.isNa(i)) {
          newVals[i] = null;
        } else {
          newVals[i] = doOperation(storage.getItem(i), argLong);
        }

        context.safepoint();
      }

      return new StringStorage(newVals, size, (TextType) storage.getType());
    } else {
      throw new UnexpectedTypeException("a Text");
    }
  }

  @Override
  public Storage<?> runZip(
      SpecializedStorage<String> storage,
      Storage<?> arg,
      MapOperationProblemAggregator problemAggregator) {
    if (arg instanceof LongStorage v) {
      int size = storage.size();
      String[] newVals = new String[size];
      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        if (storage.isNa(i) || v.isNa(i)) {
          newVals[i] = null;
        } else {
          newVals[i] = doOperation(storage.getItem(i), v.getItem(i));
        }

        context.safepoint();
      }

      return new StringStorage(newVals, size, (TextType) storage.getType());
    } else {
      throw new UnexpectedTypeException("a Text column");
    }
  }
}
