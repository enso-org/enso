package org.enso.table.data.column.operation.map.text;

import org.enso.table.data.column.builder.Builder;
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
  public Storage<String> runBinaryMap(
      SpecializedStorage<String> storage,
      Object arg,
      MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return storage;
    } else if (arg instanceof String argString) {
      TextType argumentType = TextType.preciseTypeForValue(argString);
      TextType newType = computeResultType((TextType) storage.getType(), argumentType);

      long size = storage.getSize();
      var builder = Builder.getForText(newType, size);
      Context context = Context.getCurrent();
      for (long i = 0; i < size; i++) {
        if (storage.isNothing(i)) {
          builder.append(argString);
        } else {
          builder.append(doString(storage.getItemBoxed(i), argString));
        }

        context.safepoint();
      }

      return builder.seal();
    } else {
      throw new UnexpectedTypeException("a Text");
    }
  }

  @Override
  public Storage<String> runZip(
      SpecializedStorage<String> storage,
      Storage<?> arg,
      MapOperationProblemAggregator problemAggregator) {
    if (arg instanceof StringStorage v) {
      long size = storage.getSize();
      TextType newType = computeResultType((TextType) storage.getType(), v.getType());
      var builder = Builder.getForText(newType, size);
      Context context = Context.getCurrent();
      for (long i = 0; i < size; i++) {
        String a = storage.getItemBoxed(i);
        String b = v.getItemBoxed(i);
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

        builder.append(r);
        context.safepoint();
      }

      return builder.seal();
    } else {
      throw new UnexpectedTypeException("a Text column");
    }
  }
}
