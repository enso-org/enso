package org.enso.table.data.column.operation.map.text;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public abstract class StringStringOp
    extends BinaryMapOperation<String, SpecializedStorage<String>> {
  public StringStringOp(String name) {
    super(name);
  }

  protected abstract String doString(String a, String b);

  protected abstract TextType computeResultType(TextType a, TextType b);

  @Override
  public Storage<?> runBinaryMap(
      SpecializedStorage<String> storage,
      Object arg,
      MapOperationProblemAggregator problemAggregator) {
    int size = storage.size();
    if (arg == null) {
      var builder = Builder.getForText(size, TextType.VARIABLE_LENGTH);
      builder.appendNulls(size);
      return builder.seal();
    } else if (arg instanceof String argString) {
      TextType argumentType = TextType.preciseTypeForValue(argString);
      TextType newType = computeResultType((TextType) storage.getType(), argumentType);

      var builder = Builder.getForText(size, newType);
      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        if (storage.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          builder.append(doString(storage.getBoxed(i), argString));
        }

        context.safepoint();
      }

      return builder.seal();
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
      TextType newType = computeResultType((TextType) storage.getType(), v.getType());
      int size = storage.size();
      var builder = Builder.getForText(size, newType);
      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        if (storage.isNothing(i) || v.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          builder.append(doString(storage.getBoxed(i), v.getBoxed(i)));
        }

        context.safepoint();
      }

      return builder.seal();
    } else {
      throw new UnexpectedTypeException("a Text column");
    }
  }
}
