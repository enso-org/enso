package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.MixedBuilder;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.graalvm.polyglot.Context;

/**
 * A column backing Mixed storage.
 *
 * <p>It stores the objects as Object[] and reports a Mixed type, but it may specialize itself to a
 * more precise type if all values have a common type, and will allow operations on this more
 * specific type.
 */
public final class MixedStorage extends ObjectStorage {
  private StorageType inferredType = null;

  /**
   * Holds a specialized storage for the inferred type, if available.
   *
   * <p>This storage may provide vectorized implementations of operations for more specific types.
   * Used when the Mixed type column pretends to be of another type, by reporting a more specialized
   * inferred type. This allows it to support operations of that type.
   *
   * <p>Once the specialized storage is first computed, all vectorized operations will be forwarded
   * to it - assuming that it will most likely provide more efficient implementations, even for
   * operations that are also defined on ObjectStorage.
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

  private boolean isNumeric(StorageType type) {
    return type instanceof IntegerType
        || type instanceof FloatType
        || type instanceof BigIntegerType;
  }

  private StorageType commonNumericType(StorageType a, StorageType b) {
    assert isNumeric(a);
    assert isNumeric(b);
    if (a instanceof FloatType || b instanceof FloatType) {
      return FloatType.FLOAT_64;
    } else if (a instanceof BigIntegerType || b instanceof BigIntegerType) {
      return BigIntegerType.INSTANCE;
    } else {
      assert a instanceof IntegerType;
      assert b instanceof IntegerType;
      return IntegerType.INT_64;
    }
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
        } else if (!currentType.equals(itemType)) {
          if (isNumeric(currentType) && isNumeric(itemType)) {
            currentType = commonNumericType(currentType, itemType);
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

  private enum VectorizedOperationAvailability {
    NOT_AVAILABLE,
    AVAILABLE_IN_SPECIALIZED_STORAGE,
    AVAILABLE_IN_SUPER
  }

  /**
   * The resolution depends on the following philosophy:
   *
   * <p>1. If the inferred storage is already cached, we prefer to use it since it will provide us
   * with a more efficient implementation.
   *
   * <p>2. If it is not yet cached, we do not want to compute it (since it is costly) unless it is
   * necessary - if our basic storage already provides the operation, we will use that
   * implementation - even if it may not be as fast as a specialized one, the cost of computing the
   * precise storage may just not be worth it. If our storage does not provide the operation, we now
   * need to try getting the inferred storage, to check if it may provide it.
   */
  private VectorizedOperationAvailability resolveUnaryOp(String name) {
    // Shortcut - if the storage is already specialized - we prefer it.
    if (cachedInferredStorage != null && cachedInferredStorage.isUnaryOpVectorized(name)) {
      return VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE;
    }

    // Otherwise, we try to avoid specializing if not yet necessary.
    if (super.isUnaryOpVectorized(name)) {
      return VectorizedOperationAvailability.AVAILABLE_IN_SUPER;
    } else {
      // But if our storage does not provide the operation, we have to try checking the other one.
      if (getInferredStorage() != null && getInferredStorage().isUnaryOpVectorized(name)) {
        return VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE;
      } else {
        return VectorizedOperationAvailability.NOT_AVAILABLE;
      }
    }
  }

  /** {@see resolveUnaryOp} for explanations. */
  private VectorizedOperationAvailability resolveBinaryOp(String name) {
    // Shortcut - if the storage is already specialized - we prefer it.
    if (cachedInferredStorage != null && cachedInferredStorage.isBinaryOpVectorized(name)) {
      return VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE;
    }

    // Otherwise, we try to avoid specializing if not yet necessary.
    if (super.isBinaryOpVectorized(name)) {
      return VectorizedOperationAvailability.AVAILABLE_IN_SUPER;
    } else {
      // But if our storage does not provide the operation, we have to try checking the other one.
      if (getInferredStorage() != null && getInferredStorage().isBinaryOpVectorized(name)) {
        return VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE;
      } else {
        return VectorizedOperationAvailability.NOT_AVAILABLE;
      }
    }
  }

  /** {@see resolveUnaryOp} for explanations. */
  private VectorizedOperationAvailability resolveTernaryOp(String name) {
    // Shortcut - if the storage is already specialized - we prefer it.
    if (cachedInferredStorage != null && cachedInferredStorage.isTernaryOpVectorized(name)) {
      return VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE;
    }

    // Otherwise, we try to avoid specializing if not yet necessary.
    if (super.isTernaryOpVectorized(name)) {
      return VectorizedOperationAvailability.AVAILABLE_IN_SUPER;
    } else {
      // But if our storage does not provide the operation, we have to try checking the other one.
      if (getInferredStorage() != null && getInferredStorage().isTernaryOpVectorized(name)) {
        return VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE;
      } else {
        return VectorizedOperationAvailability.NOT_AVAILABLE;
      }
    }
  }

  @Override
  public boolean isUnaryOpVectorized(String name) {
    return resolveUnaryOp(name) != VectorizedOperationAvailability.NOT_AVAILABLE;
  }

  @Override
  public Storage<?> runVectorizedUnaryMap(String name, MapOperationProblemBuilder problemBuilder) {
    if (resolveUnaryOp(name) == VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE) {
      return getInferredStorage().runVectorizedUnaryMap(name, problemBuilder);
    } else {
      // Even if the operation is not available, we rely on super to report an exception.
      return super.runVectorizedUnaryMap(name, problemBuilder);
    }
  }

  @Override
  public boolean isBinaryOpVectorized(String name) {
    return resolveBinaryOp(name) != VectorizedOperationAvailability.NOT_AVAILABLE;
  }

  @Override
  public Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemBuilder problemBuilder) {
    if (resolveBinaryOp(name) == VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE) {
      return getInferredStorage().runVectorizedBinaryMap(name, argument, problemBuilder);
    } else {
      // Even if the operation is not available, we rely on super to report an exception.
      return super.runVectorizedBinaryMap(name, argument, problemBuilder);
    }
  }

  @Override
  public boolean isTernaryOpVectorized(String name) {
    return resolveTernaryOp(name) != VectorizedOperationAvailability.NOT_AVAILABLE;
  }

  @Override
  public Storage<?> runVectorizedTernaryMap(
      String name, Object argument0, Object argument1, MapOperationProblemBuilder problemBuilder) {
    if (resolveTernaryOp(name)
        == VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE) {
      return getInferredStorage()
          .runVectorizedTernaryMap(name, argument0, argument1, problemBuilder);
    } else {
      // Even if the operation is not available, we rely on super to report an exception.
      return super.runVectorizedTernaryMap(name, argument0, argument1, problemBuilder);
    }
  }

  @Override
  public Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder) {
    if (resolveBinaryOp(name) == VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE) {
      return getInferredStorage().runVectorizedZip(name, argument, problemBuilder);
    } else {
      // Even if the operation is not available, we rely on super to report an exception.
      return super.runVectorizedZip(name, argument, problemBuilder);
    }
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new MixedBuilder(capacity);
  }

  @Override
  public Storage<?> tryGettingMoreSpecializedStorage() {
    var inferredStorage = getInferredStorage();
    return inferredStorage != null ? inferredStorage : this;
  }
}
