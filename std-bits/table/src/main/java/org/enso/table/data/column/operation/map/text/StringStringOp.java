package org.enso.table.data.column.operation.map.text;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
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
  public Storage<String> runBinaryMap(
      SpecializedStorage<String> storage,
      Object arg,
      MapOperationProblemAggregator problemAggregator) {
    long size = storage.getSize();
    if (arg == null) {
      return StringBuilder.makeEmpty(TextType.VARIABLE_LENGTH, size);
    } else if (arg instanceof String argString) {
      TextType argumentType = TextType.preciseTypeForValue(argString);
      TextType newType = computeResultType((TextType) storage.getType(), argumentType);

      var builder = Builder.getForText(newType, size);
      Context context = Context.getCurrent();
      for (long i = 0; i < size; i++) {
        if (storage.isNothing(i)) {
          builder.appendNulls(1);
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
    if (TextType.VARIABLE_LENGTH.isOfType(arg.getType())) {
      var v = TextType.VARIABLE_LENGTH.asTypedStorage(arg);
      TextType newType = computeResultType((TextType) storage.getType(), (TextType) v.getType());
      return (Storage<String>)
          StorageIterators.zipOverStorages(
              storage,
              v,
              s -> Builder.getForText(newType, s),
              true,
              (index, left, right) -> doString(left, right));
    } else {
      throw new UnexpectedTypeException("a Text column");
    }
  }
}
