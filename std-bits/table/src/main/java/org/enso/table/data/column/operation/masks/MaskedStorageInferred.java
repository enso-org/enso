package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.cast.CastOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.problems.BlackholeProblemAggregator;

final class MaskedStorageInferred<T> extends MaskedStorage<T>
    implements ColumnStorageWithInferredStorage {
  private ColumnStorage<?> cachedInferredStorage = null;
  private boolean hasComputedInferredStorage = false;

  MaskedStorageInferred(ColumnStorage<T> parent, IndexMapper indexMapper) {
    super(parent, indexMapper);
  }

  @Override
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
