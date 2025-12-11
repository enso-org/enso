package org.enso.table.data.column.storage;

import java.lang.foreign.MemorySegment;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Iterator;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.util.ImmutableBitSet;

public class TypedStorage<T> extends Storage<T> {
  private final T[] data;
  private final ColumnStorage<?> proxy;
  private ByteBuffer offheapBuffer;
  private ImmutableBitSet validitySet;

  /**
   * @param data the underlying data
   */
  public TypedStorage(StorageType<T> type, T[] data) {
    this(type, data, null);
  }

  public TypedStorage(StorageType<T> type, T[] data, ColumnStorage<?> proxy) {
    super(type);
    this.data = data;
    this.proxy = proxy;
  }

  @Override
  public long addressOfData() {
    if (offheapBuffer == null && getType() instanceof TextType) {
      var textSize = 0;
      for (var value : data) {
        if (value instanceof String s) {
          textSize += s.getBytes(StandardCharsets.UTF_8).length;
        } else {
          if (value != null) {
            return 0L;
          }
        }
      }

      var indexSize = data.length * Integer.BYTES + Integer.BYTES;
      var fullSize = indexSize + textSize;
      var buf = ByteBuffer.allocateDirect(fullSize).order(ByteOrder.LITTLE_ENDIAN);
      var index = buf.asIntBuffer().slice(0, data.length + 1);
      buf.position(indexSize);
      var validity = new BitSet();
      for (var value : data) {
        var at = index.position();
        index.put(buf.position() - indexSize);
        if (value instanceof String s) {
          validity.set(at, true);
        } else {
          validity.set(at, false);
          continue;
        }
        buf.put(s.getBytes(StandardCharsets.UTF_8));
      }
      assert buf.limit() == buf.position();
      index.put(buf.position() - indexSize);
      assert index.position() == index.limit();
      buf.flip();
      assert buf.position() == 0;
      assert buf.limit() == fullSize;
      offheapBuffer = buf;
      validitySet = new ImmutableBitSet(validity, data.length);
    }
    if (offheapBuffer != null) {
      return MemorySegment.ofBuffer(offheapBuffer).address();
    }
    return 0L;
  }

  @Override
  public long addressOfValidity() {
    if (addressOfData() != 0L) {
      return MemorySegment.ofBuffer(validitySet.rawData()).address();
    }
    return 0L;
  }

  @Override
  public final long getSize() {
    return data.length;
  }

  /**
   * @param idx an index
   * @return the data item contained at the given index.
   */
  public T getItemBoxed(long idx) {
    if (idx < 0 || idx >= data.length) {
      throw new IndexOutOfBoundsException(idx);
    }
    return data[(int) idx];
  }

  public T[] getData() {
    return data;
  }

  @Override
  public Iterator<T> iterator() {
    return Arrays.stream(data).iterator();
  }
}
