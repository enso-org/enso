package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.UnaryMapOperation;

import java.util.BitSet;

/** A column storing arbitrary objects. */
public class ObjectStorage extends SpecializedStorage<Object> {
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
  public long getType() {
    return Type.OBJECT;
  }

  private static final MapOpStorage<SpecializedStorage<Object>> ops = buildObjectOps();

  static <S extends SpecializedStorage<?>> MapOpStorage<S> buildObjectOps() {
    MapOpStorage<S> ops = new MapOpStorage<>();
    ops.add(
        new UnaryMapOperation<>(Maps.IS_MISSING) {
          @Override
          protected Storage run(S storage) {
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
