package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.graalvm.polyglot.Context;

/**
 * A column backing Mixed storage.
 *
 * <p>It stores the objects as Object[] and reports a Mixed type, but it may specialize itself to a
 * more precise type if all values have a common type, and will allow operations on this more
 * specific type.
 */
public final class MixedStorage extends ObjectStorage implements ColumnStorageWithInferredStorage {

  /**
   * Holds a specialized storage for the inferred type, if available.
   *
   * <p>This storage may provide vectorized implementations of operations for more specific types.
   * Used when the Mixed type column pretends to be of another type, by reporting a more specialized
   * inferred type. This allows it to support operations of that type.
   *
   * <p>Once the specialized storage is first computed, all vectorized operations will be forwarded
   * to it - assuming that it will most likely provide more efficient implementations.
   */
  private Storage<?> cachedInferredStorage = null;

  private boolean hasSpecializedStorageBeenInferred = false;

  /**
   * @param data the underlying data
   */
  public MixedStorage(Object[] data) {
    super(data);
  }

  @Override
  protected SpecializedStorage<Object> newInstance(Object[] data) {
    return new MixedStorage(data);
  }

  @Override
  public StorageType<?> inferPreciseType(PreciseTypeOptions options) {
    if (options.equals(PreciseTypeOptions.DEFAULT)) {
      if (cachedDefaultPreciseType == null) {
        cachedDefaultPreciseType = computePreciseType(PreciseTypeOptions.DEFAULT);
      }
      return cachedDefaultPreciseType;
    }

    return computePreciseType(options);
  }

  private StorageType<?> cachedDefaultPreciseType = null;

  private StorageType<?> computePreciseType(PreciseTypeOptions options) {
    StorageType<?> currentType = null;

    Context context = Context.getCurrent();
    for (long i = 0; i < getSize(); i++) {
      var item = getItemBoxed(i);
      if (item == null) {
        continue;
      }

      var itemType = StorageType.forBoxedItem(item, options);
      if (currentType == null) {
        currentType = itemType;
      } else {
        currentType = reconcileTypes(currentType, itemType);
      }

      if (currentType instanceof AnyObjectType) {
        // The type won't get any wider so no point in continuing.
        break;
      }

      context.safepoint();
    }

    return currentType == null ? AnyObjectType.INSTANCE : currentType;
  }

  private static StorageType<?> reconcileTypes(
      StorageType<?> currentType, StorageType<?> itemType) {
    if (currentType.equals(itemType)) {
      return currentType;
    } else {
      if (currentType instanceof TextType currentTextType
          && itemType instanceof TextType itemTextType) {
        return TextType.maxType(currentTextType, itemTextType);
      } else if (currentType.isNumeric() && itemType.isNumeric()) {
        return commonNumericType(currentType, itemType);
      } else {
        return AnyObjectType.INSTANCE;
      }
    }
  }

  private static StorageType<?> commonNumericType(StorageType<?> a, StorageType<?> b) {
    assert a.isNumeric();
    assert b.isNumeric();
    if (a instanceof BigDecimalType || b instanceof BigDecimalType) {
      return BigDecimalType.INSTANCE;
    } else if (a instanceof FloatType || b instanceof FloatType) {
      return FloatType.FLOAT_64;
    } else if (a instanceof BigIntegerType || b instanceof BigIntegerType) {
      return BigIntegerType.INSTANCE;
    } else {
      if (a instanceof IntegerType aInt && b instanceof IntegerType bInt) {
        return IntegerType.commonType(aInt, bInt);
      } else {
        throw new IllegalStateException("Unexpected numeric types: " + a + " and " + b);
      }
    }
  }

  public Storage<?> getInferredStorage() {
    if (!hasSpecializedStorageBeenInferred) {
      StorageType<?> inferredType = inferPreciseType(PreciseTypeOptions.DEFAULT);
      if (inferredType instanceof AnyObjectType) {
        cachedInferredStorage = null;
      } else {
        // Any problems will be discarded - this is not a real conversion but just an approximation
        // for purposes of a computation.
        Builder builder =
            Builder.getForType(inferredType, getSize(), BlackholeProblemAggregator.INSTANCE);
        for (long i = 0; i < getSize(); i++) {
          builder.append(getItemBoxed(i));
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

  @Override
  public boolean isBinaryOpVectorized(String name) {
    return resolveBinaryOp(name) != VectorizedOperationAvailability.NOT_AVAILABLE;
  }

  @Override
  public Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemAggregator problemAggregator) {
    if (resolveBinaryOp(name) == VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE) {
      return getInferredStorage().runVectorizedBinaryMap(name, argument, problemAggregator);
    } else {
      // Even if the operation is not available, we rely on super to report an exception.
      return super.runVectorizedBinaryMap(name, argument, problemAggregator);
    }
  }

  @Override
  public Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemAggregator problemAggregator) {
    if (resolveBinaryOp(name) == VectorizedOperationAvailability.AVAILABLE_IN_SPECIALIZED_STORAGE) {
      return getInferredStorage().runVectorizedZip(name, argument, problemAggregator);
    } else {
      // Even if the operation is not available, we rely on super to report an exception.
      return super.runVectorizedZip(name, argument, problemAggregator);
    }
  }

  @Override
  public Storage<?> tryGettingMoreSpecializedStorage() {
    var inferredStorage = getInferredStorage();
    return inferredStorage != null ? inferredStorage : this;
  }
}
