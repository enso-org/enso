package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.StorageType;
import org.graalvm.polyglot.Context;

import java.util.BitSet;

/**
 * A column storing arbitrary Java objects.
 */
public sealed class ObjectStorage extends SpecializedStorage<Object> permits MixedStorage {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public ObjectStorage(Object[] data, int size) {
    super(data, size, buildObjectOps());
  }

  @Override
  protected SpecializedStorage<Object> newInstance(Object[] data, int size) {
    return new ObjectStorage(data, size);
  }

  @Override
  protected Object[] newUnderlyingArray(int size) {
    return new Object[size];
  }

  @Override
  public StorageType getType() {
    return AnyObjectType.INSTANCE;
  }

  public static <T, S extends SpecializedStorage<T>> MapOperationStorage<T, S> buildObjectOps() {
    MapOperationStorage<T, S> ops = new MapOperationStorage<>();
    ops.add(
        new UnaryMapOperation<>(Maps.IS_NOTHING) {
          @Override
          protected BoolStorage runUnaryMap(S storage, MapOperationProblemAggregator problemAggregator) {
            Context context = Context.getCurrent();
            BitSet r = new BitSet();
            for (int i = 0; i < storage.size; i++) {
              if (storage.data[i] == null) {
                r.set(i);
              }

              context.safepoint();
            }
            return new BoolStorage(r, new BitSet(), storage.size, false);
          }
        });
    return ops;
  }
}
