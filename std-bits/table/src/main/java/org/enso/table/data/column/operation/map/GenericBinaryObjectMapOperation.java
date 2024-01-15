package org.enso.table.data.column.operation.map;

import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.ObjectBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public abstract class GenericBinaryObjectMapOperation<
        Input1Type,
        Input1StorageType extends Storage<Input1Type>,
        Input2Type,
        Input2StorageType extends Storage<Input2Type>,
        OutputType>
    extends BinaryMapOperation<Input1Type, Input1StorageType> {

  protected GenericBinaryObjectMapOperation(
      String name,
      Class<Input1Type> input1TypeClass,
      Class<? extends Input1StorageType> input1StorageTypeClass,
      Class<Input2Type> input2TypeClass,
      Class<? extends Input2StorageType> input2StorageTypeClass) {
    super(name);
    this.input1TypeClass = input1TypeClass;
    this.input1StorageTypeClass = input1StorageTypeClass;
    this.input2TypeClass = input2TypeClass;
    this.input2StorageTypeClass = input2StorageTypeClass;
  }

  private final Class<Input1Type> input1TypeClass;
  private final Class<? extends Input1StorageType> input1StorageTypeClass;
  private final Class<Input2Type> input2TypeClass;
  private final Class<? extends Input2StorageType> input2StorageTypeClass;

  protected Builder createOutputBuilder(int size) {
    return new ObjectBuilder(size);
  }

  protected abstract OutputType run(Input1Type value, Input2Type other);

  @Override
  public Storage<?> runBinaryMap(
      Input1StorageType storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    arg = Polyglot_Utils.convertPolyglotValue(arg);
    if (arg == null) {
      int n = storage.size();
      Builder builder = createOutputBuilder(n);
      builder.appendNulls(n);
      return builder.seal();
    } else if (input2TypeClass.isInstance(arg)) {
      Input2Type casted = input2TypeClass.cast(arg);
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
          "a " + input1TypeClass.getName() + " but got " + arg.getClass().getName());
    }
  }

  @Override
  public Storage<?> runZip(
      Input1StorageType storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    if (input1StorageTypeClass.isInstance(arg)) {
      Input2StorageType otherCasted = input2StorageTypeClass.cast(arg);
      int n = storage.size();
      Builder builder = createOutputBuilder(n);
      Context context = Context.getCurrent();
      for (int i = 0; i < n; ++i) {
        if (storage.isNa(i) || otherCasted.isNa(i)) {
          builder.appendNulls(1);
        } else {
          Input1Type left = storage.getItemBoxed(i);
          Input2Type right = otherCasted.getItemBoxed(i);
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
          Input1Type left = storage.getItemBoxed(i);
          Object right = arg.getItemBoxed(i);
          if (input2TypeClass.isInstance(right)) {
            OutputType result = run(left, input2TypeClass.cast(right));
            builder.append(result);
          } else {
            throw new UnexpectedTypeException(
                "Got a mixed storage where values were assumed to be of type "
                    + input1TypeClass.getName()
                    + ", but got a value of type "
                    + right.getClass().getName());
          }
        }

        context.safepoint();
      }
      return builder.seal();
    } else {
      throw new UnexpectedTypeException(
          "a " + input1StorageTypeClass.getName() + " or a mixed storage");
    }
  }
}
