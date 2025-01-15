package org.enso.table.data.column.operation.map.text;

import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public abstract class CoalescingStringStringOp extends StringStringOp {
  public CoalescingStringStringOp(String name) {
    super(name);
  }

  @Override
  public Storage<?> runBinaryMap(
      SpecializedStorage<String> storage,
      Object arg,
      MapOperationProblemAggregator problemAggregator) {
    int size = storage.size();
    if (arg == null) {
      return storage;
    } else if (arg instanceof String argString) {
      String[] newVals = new String[size];
      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        if (storage.isNothing(i)) {
          newVals[i] = argString;
        } else {
          newVals[i] = doString(storage.getItem(i), argString);
        }

        context.safepoint();
      }

      TextType argumentType = TextType.preciseTypeForValue(argString);
      TextType newType = computeResultType((TextType) storage.getType(), argumentType);
      return new StringStorage(newVals, size, newType);
    } else {
      throw new UnexpectedTypeException("a Text");
    }
  }

  @Override
  public Storage<?> runZip(
      SpecializedStorage<String> storage,
      Storage<?> arg,
      MapOperationProblemAggregator problemAggregator) {
    if (arg instanceof StringStorage v) {
      int size = storage.size();
      String[] newVals = new String[size];
      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        String a = storage.getItem(i);
        String b = v.getItem(i);
        String r;
        if (a == null && b == null) {
          r = null;
        } else {
          if (a == null) {
            r = b;
          } else if (b == null) {
            r = a;
          } else {
            r = doString(a, b);
          }
        }

        newVals[i] = r;

        context.safepoint();
      }

      TextType newType = computeResultType((TextType) storage.getType(), v.getType());
      return new StringStorage(newVals, size, newType);
    } else {
      throw new UnexpectedTypeException("a Text column");
    }
  }
}
