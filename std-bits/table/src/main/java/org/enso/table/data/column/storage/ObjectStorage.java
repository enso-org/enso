package org.enso.table.data.column.storage;

import java.util.BitSet;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.ObjectBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;

/** A column storing arbitrary objects. */
public final class ObjectStorage extends SpecializedStorage<Object> {
  private StorageType inferredType = null;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public ObjectStorage(Object[] data, int size) {
    super(data, size, ops);
    inferredType = null;
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
  public StorageType inferPreciseType() {
    if (inferredType == null) {
      StorageType currentType = null;

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
      }

      inferredType = currentType == null ? AnyObjectType.INSTANCE : currentType;
    }

    return inferredType;
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new ObjectBuilder(capacity);
  }

  private static final MapOpStorage<Object, SpecializedStorage<Object>> ops = buildObjectOps();

  public static <T, S extends SpecializedStorage<T>> MapOpStorage<T, S> buildObjectOps() {
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
