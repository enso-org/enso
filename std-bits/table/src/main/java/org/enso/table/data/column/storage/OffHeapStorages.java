package org.enso.table.data.column.storage;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.nio.charset.StandardCharsets;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.BitSet;

final class OffHeapStorages {
  private OffHeapStorages() {}

  static ByteBuffer toArrowTimeOfDayBuffer(Object[] data, BitSet validity) {
    int fullSize = data.length * Long.BYTES;
    ByteBuffer buf = ByteBuffer.allocateDirect(fullSize).order(ByteOrder.LITTLE_ENDIAN);
    int at = 0;
    for (Object value : data) {
      if (value instanceof LocalTime s) {
        buf.putLong(s.toNanoOfDay());
        validity.set(at, true);
      } else {
        buf.putLong(0);
        validity.set(at, false);
      }
      at++;
    }
    assert buf.limit() == buf.position();
    buf.flip();
    assert buf.position() == 0;
    assert buf.limit() == fullSize;
    return buf;
  }

  static ByteBuffer toDateTimeBuffer(Object[] data, BitSet validity) {
    int fullSize = data.length * Long.BYTES;
    ByteBuffer buf = ByteBuffer.allocateDirect(fullSize).order(ByteOrder.LITTLE_ENDIAN);
    int at = 0;
    for (Object value : data) {
      if (value instanceof ZonedDateTime s) {
        buf.putLong(s.toInstant().toEpochMilli());
        validity.set(at, true);
      } else {
        buf.putLong(0);
        validity.set(at, false);
      }
      at++;
    }
    assert buf.limit() == buf.position();
    buf.flip();
    assert buf.position() == 0;
    assert buf.limit() == fullSize;
    return buf;
  }

  static ByteBuffer toArrowTextBuffer(Object[] data, BitSet validity) {
    int textSize = 0;
    for (Object value : data) {
      if (value instanceof String s) {
        textSize += s.getBytes(StandardCharsets.UTF_8).length;
      } else {
        if (value != null) {
          return null;
        }
      }
    }
    int indexSize = data.length * Integer.BYTES + Integer.BYTES;
    int fullSize = indexSize + textSize;
    ByteBuffer buf = ByteBuffer.allocateDirect(fullSize).order(ByteOrder.LITTLE_ENDIAN);
    IntBuffer index = buf.asIntBuffer().slice(0, data.length + 1);
    buf.position(indexSize);
    for (Object value : data) {
      int at = index.position();
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
    return buf;
  }
}
