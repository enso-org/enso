package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.ObjectBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.StorageType;

import java.util.BitSet;

/** A column storing arbitrary objects. */
public final class ObjectStorage extends SpecializedStorage<Object> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public ObjectStorage(Object[] data, int size) {
    super(data, size, ops);
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

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new ObjectBuilder(capacity);
  }

  private static final MapOpStorage<Object, SpecializedStorage<Object>> ops = buildObjectOps();

  static <T, S extends SpecializedStorage<T>> MapOpStorage<T, S> buildObjectOps() {
    MapOpStorage<T, S> ops = new MapOpStorage<>();
    ops.add(
        new UnaryMapOperation<>(Maps.IS_NOTHING) {
          @Override
          protected BoolStorage run(S storage) {
            BitSet r = new BitSet();
            for (int i = 0; i < storage.size; i++) {
              if (storage.data[i] == null) {
                r.set(i);
              }
            }
            return new BoolStorage(r, new BitSet(), storage.size, false);
          }
        });
    return ops;
  }
}
