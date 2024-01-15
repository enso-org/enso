package org.enso.table.data.column.operation.map;

import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.ObjectBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public abstract class GenericBinaryObjectMapOperation<
        InputType,
        InputStorageType extends Storage<InputType>,
        ArgType,
        ArgStorageType extends Storage<ArgType>,
        OutputType>
    extends BinaryMapOperation<InputType, InputStorageType> {

  protected GenericBinaryObjectMapOperation(
      String name,
      Class<ArgType> argTypeClass,
      Class<? extends ArgStorageType> argStorageTypeClass) {
    super(name);
    this.argTypeClass = argTypeClass;
    this.argStorageTypeClass = argStorageTypeClass;
  }

  private final Class<ArgType> argTypeClass;
  private final Class<? extends ArgStorageType> argStorageTypeClass;

  protected Builder createOutputBuilder(int size) {
    return new ObjectBuilder(size);
  }

  protected abstract OutputType run(InputType value, ArgType other);

  @Override
  public Storage<?> runBinaryMap(
      InputStorageType storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    arg = Polyglot_Utils.convertPolyglotValue(arg);
    if (arg == null) {
      int n = storage.size();
      Builder builder = createOutputBuilder(n);
      builder.appendNulls(n);
      return builder.seal();
    } else if (argTypeClass.isInstance(arg)) {
      ArgType casted = argTypeClass.cast(arg);
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
          "a " + argTypeClass.getName() + " but got " + arg.getClass().getName());
    }
  }

  @Override
  public Storage<?> runZip(
      InputStorageType storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    if (argStorageTypeClass.isInstance(arg)) {
      ArgStorageType otherCasted = argStorageTypeClass.cast(arg);
      int n = storage.size();
      Builder builder = createOutputBuilder(n);
      Context context = Context.getCurrent();
      for (int i = 0; i < n; ++i) {
        if (storage.isNa(i) || otherCasted.isNa(i)) {
          builder.appendNulls(1);
        } else {
          InputType left = storage.getItemBoxed(i);
          ArgType right = otherCasted.getItemBoxed(i);
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
          if (argTypeClass.isInstance(right)) {
            OutputType result = run(left, argTypeClass.cast(right));
            builder.append(result);
          } else {
            throw new UnexpectedTypeException(
                "Got a mixed storage where values were assumed to be of type "
                    + argTypeClass.getName()
                    + ", but got a value of type "
                    + right.getClass().getName());
          }
        }

        context.safepoint();
      }
      return builder.seal();
    } else {
      throw new UnexpectedTypeException(
          "a " + argStorageTypeClass.getName() + " or a mixed storage");
    }
  }
}
