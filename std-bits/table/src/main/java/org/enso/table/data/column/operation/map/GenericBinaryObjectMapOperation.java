package org.enso.table.data.column.operation.map;

import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public abstract class GenericBinaryObjectMapOperation<
        InputType, InputStorageType extends Storage<InputType>, OutputType>
    extends BinaryMapOperation<InputType, InputStorageType> {

  protected GenericBinaryObjectMapOperation(
      String name,
      Class<InputType> inputTypeClass,
      Class<? extends InputStorageType> inputStorageTypeClass) {
    super(name);
    this.inputTypeClass = inputTypeClass;
    this.inputStorageTypeClass = inputStorageTypeClass;
  }

  private final Class<InputType> inputTypeClass;
  private final Class<? extends InputStorageType> inputStorageTypeClass;

  protected abstract Builder createOutputBuilder(int size);

  protected abstract OutputType run(InputType value, InputType other);

  @Override
  public Storage<?> runBinaryMap(
      InputStorageType storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    arg = Polyglot_Utils.convertPolyglotValue(arg);
    if (arg == null) {
      int n = storage.size();
      Builder builder = createOutputBuilder(n);
      builder.appendNulls(n);
      return builder.seal();
    } else if (inputTypeClass.isInstance(arg)) {
      InputType casted = inputTypeClass.cast(arg);
      int n = storage.size();
      Builder builder = createOutputBuilder(n);
      Context context = Context.getCurrent();
      for (int i = 0; i < n; i++) {
        if (storage.isNa(i)) {
          builder.appendNulls(1);
        } else {
          OutputType result = run(storage.getItemBoxed(i), casted);
          builder.appendNoGrow(result);
        }

        context.safepoint();
      }
      return builder.seal();
    } else {
      throw new UnexpectedTypeException(
          "a " + inputTypeClass.getName() + " but got " + arg.getClass().getName());
    }
  }

  @Override
  public Storage<?> runZip(
      InputStorageType storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    if (inputStorageTypeClass.isInstance(arg)) {
      InputStorageType otherCasted = inputStorageTypeClass.cast(arg);
      int n = storage.size();
      Builder builder = createOutputBuilder(n);
      Context context = Context.getCurrent();
      for (int i = 0; i < n; ++i) {
        if (storage.isNa(i) || otherCasted.isNa(i)) {
          builder.appendNulls(1);
        } else {
          InputType left = storage.getItemBoxed(i);
          InputType right = otherCasted.getItemBoxed(i);
          OutputType result = run(left, right);
          builder.append(result);
        }

        context.safepoint();
      }
      return builder.seal();
    } else if (arg.getType() instanceof AnyObjectType) {
      // TODO this case may not be needed once #7231 gets implemented
      int n = storage.size();
      Builder builder = createOutputBuilder(n);
      Context context = Context.getCurrent();
      for (int i = 0; i < n; ++i) {
        if (storage.isNa(i) || arg.isNa(i)) {
          builder.appendNulls(1);
        } else {
          InputType left = storage.getItemBoxed(i);
          Object right = arg.getItemBoxed(i);
          if (inputTypeClass.isInstance(right)) {
            OutputType result = run(left, inputTypeClass.cast(right));
            builder.append(result);
          } else {
            throw new UnexpectedTypeException(
                "Got a mixed storage where values were assumed to be of type "
                    + inputTypeClass.getName()
                    + ", but got a value of type "
                    + right.getClass().getName());
          }
        }

        context.safepoint();
      }
      return builder.seal();
    } else {
      throw new UnexpectedTypeException(
          "a " + inputStorageTypeClass.getName() + " or a mixed storage");
    }
  }
}
