package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.MixedBuilder;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.graalvm.polyglot.Context;

/** A column backing Mixed storage.
 * <p>
 * It stores the objects as Object[] and reports a Mixed type, but it may
 * specialize itself to a more precise type if all values have a common type,
 * and will allow operations on this more specific type.
 */
public final class MixedStorage extends ObjectStorage {
  private StorageType inferredType = null;

  /**
   * Holds a specialized storage for the inferred type, if available.
   * <p>
   * This storage may provide vectorized implementations of operations for more specific types. Used when the Mixed type
   * column pretends to be of another type, by reporting a more specialized inferred type. This allows it to support
   * operations of that type.
   */
  private Storage<?> cachedInferredStorage = null;
  private boolean hasSpecializedStorageBeenInferred = false;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public MixedStorage(Object[] data, int size) {
    super(data, size);
    inferredType = null;
  }

  @Override
  protected SpecializedStorage<Object> newInstance(Object[] data, int size) {
    return new MixedStorage(data, size);
  }

  @Override
  public StorageType inferPreciseType() {
    if (inferredType == null) {
      StorageType currentType = null;

      Context context = Context.getCurrent();
      for (int i = 0; i < size(); i++) {
        var item = getItemBoxed(i);
        if (item == null) {
          continue;
        }

        var itemType = StorageType.forBoxedItem(item);
        if (currentType == null) {
          currentType = itemType;
        } else if (currentType != itemType) {
          // Allow mixed integer and float types in a column, returning a float.
          if ((itemType instanceof IntegerType && currentType instanceof FloatType)
              || (itemType instanceof FloatType && currentType instanceof IntegerType)) {
            currentType = FloatType.FLOAT_64;
          } else {
            currentType = AnyObjectType.INSTANCE;
          }
        }

        if (currentType instanceof AnyObjectType) {
          break;
        }

        context.safepoint();
      }

      inferredType = currentType == null ? AnyObjectType.INSTANCE : currentType;
    }

    return inferredType;
  }

  private Storage<?> getInferredStorage() {
    if (!hasSpecializedStorageBeenInferred) {
      StorageType inferredType = inferPreciseType();
      if (inferredType instanceof AnyObjectType) {
        cachedInferredStorage = null;
      } else {
        Builder builder = Builder.getForType(inferredType, size());
        for (int i = 0; i < size(); i++) {
          builder.appendNoGrow(getItemBoxed(i));
        }
        cachedInferredStorage = builder.seal();
      }
      hasSpecializedStorageBeenInferred = true;
    }

    return cachedInferredStorage;
  }

  @Override
  public boolean isUnaryOpVectorized(String name) {
    if (super.isUnaryOpVectorized(name)) {
      return true;
    } else {
      return getInferredStorage() != null && getInferredStorage().isUnaryOpVectorized(name);
    }
  }

  @Override
  public Storage<?> runVectorizedUnaryMap(String name, MapOperationProblemBuilder problemBuilder) {
    return super.runVectorizedUnaryMap(name, problemBuilder);
  }

  @Override
  public boolean isBinaryOpVectorized(String name) {
    if (super.isBinaryOpVectorized(name)) {
      return true;
    } else {
      return getInferredStorage() != null && getInferredStorage().isBinaryOpVectorized(name);
    }
  }

  @Override
  public Storage<?> runVectorizedBiMap(String name, Object argument, MapOperationProblemBuilder problemBuilder) {
    return super.runVectorizedBiMap(name, argument, problemBuilder);
  }

  @Override
  public Storage<?> runVectorizedZip(String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder) {
    return super.runVectorizedZip(name, argument, problemBuilder);
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new MixedBuilder(capacity);
  }
}
