package org.enso.table.data.column.operation.map.text;

import org.enso.table.data.column.builder.Builder;
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
  public Storage<String> runBinaryMap(
      SpecializedStorage<String> storage,
      Object arg,
      MapOperationProblemAggregator problemAggregator) {
    long size = storage.getSize();
    if (arg == null) {
      return StringStorage.makeEmpty((TextType) storage.getType(), size);
    } else if (arg instanceof Long argLong) {
      var builder = Builder.getForText((TextType) storage.getType(), size);
      Context context = Context.getCurrent();
      for (long i = 0; i < size; i++) {
        if (storage.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          builder.append(doOperation(storage.getItemBoxed(i), argLong));
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
    if (arg instanceof LongStorage v) {
      long size = storage.getSize();
      var builder = Builder.getForText(TextType.VARIABLE_LENGTH, size);
      Context context = Context.getCurrent();
      for (long i = 0; i < size; i++) {
        if (storage.isNothing(i) || v.isNothing(i)) {
          builder.appendNulls(1);
        } else {
          builder.append(doOperation(storage.getItemBoxed(i), v.getItemBoxed(i)));
        }
        context.safepoint();
      }
      return builder.seal();
    } else {
      throw new UnexpectedTypeException("a Text column");
    }
  }
}
