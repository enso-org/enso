package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.cast.CastOperation;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.problems.BlackholeProblemAggregator;

/**
 * A column backing Mixed storage.
 *
 * <p>It stores the objects as Object[] and reports a Mixed type, but it may specialize itself to a
 * more precise type if all values have a common type, and will allow operations on this more
 * specific type.
 */
public final class MixedStorage extends TypedStorage<Object>
    implements ColumnStorageWithInferredStorage {
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
  private ColumnStorage<?> cachedInferredStorage = null;

  private boolean hasComputedInferredStorage = false;

  /**
   * @param data the underlying data
   */
  public MixedStorage(Object[] data) {
    super(AnyObjectType.INSTANCE, data);
  }

  public ColumnStorage<?> getInferredStorage() {
    if (!hasComputedInferredStorage) {
      var inferredType = CastOperation.reconcileObjectStorage(this);
      cachedInferredStorage =
          (inferredType instanceof AnyObjectType)
              ? null
              : StorageIterators.buildObjectOverStorage(
                  this,
                  true,
                  Builder.getForType(inferredType, getSize(), BlackholeProblemAggregator.INSTANCE),
                  (builder, index, value) -> builder.append(value));
      hasComputedInferredStorage = true;
    }

    return cachedInferredStorage;
  }
}
