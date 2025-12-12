package org.enso.table.data.column.builder;

import java.lang.foreign.MemorySegment;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.BitSet;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for string columns. */
final class StringBuilder extends TypedBuilder<String> {
  private final TextType type;

  StringBuilder(int size, TextType type) {
    super(type, new String[size]);
    this.type = type;
  }

  static StringBuilder fromAddress(int size, long data, long validity, TextType type) {
    var validityBuffer =
        MemorySegment.ofAddress(validity).reinterpret((size + 7) / 8).asByteBuffer();
    var bits = BitSet.valueOf(validityBuffer);
    var rawIndexBuffer =
        MemorySegment.ofAddress(data)
            .reinterpret(Integer.BYTES * size + Integer.BYTES)
            .asByteBuffer()
            .order(ByteOrder.LITTLE_ENDIAN);
    var indexBuffer = rawIndexBuffer.asIntBuffer();
    var textSize = indexBuffer.get(size);
    var textBuffer =
        MemorySegment.ofAddress(data + rawIndexBuffer.limit()).reinterpret(textSize).asByteBuffer();

    var b = new StringBuilder(size, type);
    for (var i = 0; i < size; i++) {
      if (bits.get(i)) {
        var from = indexBuffer.get(i);
        var to = indexBuffer.get(i + 1);
        var arr = new byte[to - from];
        textBuffer.get(from, arr);
        var s = new String(arr, StandardCharsets.UTF_8);
        b.append(s);
      } else {
        b.appendNulls(1);
      }
    }
    return b;
  }

  @Override
  public StringBuilder append(Object o) {
    ensureSpaceToAppend();
    if (o == null) {
      appendNulls(1);
    } else {
      try {
        String str = (String) o;
        if (type.fits(str)) {
          data[currentSize++] = str;
        } else {
          throw new ValueTypeMismatchException(type, str);
        }
      } catch (ClassCastException e) {
        throw new ValueTypeMismatchException(type, o);
      }
    }
    return this;
  }

  @Override
  public boolean accepts(Object o) {
    if (o instanceof String s) {
      return type.fits(s);
    } else {
      return false;
    }
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof TextType gotType
        && type.fitsExactly(gotType)
        && storage instanceof TypedStorage<?>) {
      // This cast is safe, because storage.getType() == this.getType() == TextType iff
      // storage.T == String
      @SuppressWarnings("unchecked")
      TypedStorage<String> specializedStorage = (TypedStorage<String>) storage;
      int toCopy = (int) storage.getSize();
      System.arraycopy(specializedStorage.getData(), 0, data, currentSize, toCopy);
      currentSize += toCopy;
      return;
    }

    super.appendBulkStorage(storage);
  }

  @Override
  protected ColumnStorage<String> doSeal() {
    return new TypedStorage<>(type, data);
  }

  final Storage<String> seal(ColumnStorage<?> otherStorage, TextType type) {
    return new TypedStorage<>(type, data, otherStorage);
  }
}
