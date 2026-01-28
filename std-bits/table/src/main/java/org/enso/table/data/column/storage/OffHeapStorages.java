package org.enso.table.data.column.storage;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.BitSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class OffHeapStorages {
  private static final Logger LOGGER = LoggerFactory.getLogger(OffHeapStorages.class);

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
    var zones = new String[data.length];
    for (var i = 0; i < data.length; i++) {
      if (data[i] instanceof ZonedDateTime s) {
        zones[i] = s.getZone().getId();
      }
    }

    int zonesSize = textSize(zones);
    int indexSize = data.length * Integer.BYTES + Integer.BYTES;
    int dataSize = data.length * Long.BYTES;
    int fullSize = dataSize + indexSize + zonesSize;

    var buf = ByteBuffer.allocateDirect(fullSize).order(ByteOrder.LITTLE_ENDIAN);
    int at = 0;
    for (Object value : data) {
      if (value instanceof ZonedDateTime s) {
        var instant = s.toInstant();
        var epochSeconds = instant.toEpochMilli() / 1000;
        try {
          var epochNanoRaw = Math.multiplyExact(epochSeconds, 1_000_000_000);
          var epochNano = Math.addExact(epochNanoRaw, instant.getNano());
          buf.putLong(epochNano);
        } catch (ArithmeticException ex) {
          LOGGER.warn("Cannot convert " + s + " to nanoseconds since the epoch");
          return null;
        }
        validity.set(at, true);
      } else {
        buf.putLong(0);
        validity.set(at, false);
      }
      at++;
    }
    assert buf.position() == dataSize;

    var zonesBuf = buf.slice(dataSize, buf.limit() - dataSize).order(ByteOrder.LITTLE_ENDIAN);
    var zonesFilled = textFillBuffer(zonesBuf, zones, indexSize, validity);
    assert zonesBuf == zonesFilled;

    buf.flip();
    buf.limit(fullSize);
    assert buf.position() == 0;
    return buf;
  }

  static ByteBuffer toArrowTextBuffer(Object[] data, BitSet validity) {
    int textSize = textSize(data);
    if (textSize == -1) {
      return null;
    }
    int indexSize = data.length * Integer.BYTES + Integer.BYTES;
    int fullSize = indexSize + textSize;
    ByteBuffer buf = ByteBuffer.allocateDirect(fullSize).order(ByteOrder.LITTLE_ENDIAN);
    var filledBuf = textFillBuffer(buf, data, indexSize, validity);
    assert filledBuf.limit() == fullSize;
    return filledBuf;
  }

  private static ByteBuffer textFillBuffer(
      ByteBuffer buf, Object[] data, int indexSize, BitSet validity) {
    var index = buf.asIntBuffer().slice(0, data.length + 1);
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
    return buf;
  }

  private static int textSize(Object[] data) {
    int textSize = 0;
    for (Object value : data) {
      if (value instanceof String s) {
        textSize += s.getBytes(StandardCharsets.UTF_8).length;
      } else {
        if (value != null) {
          textSize = -1;
          break;
        }
      }
    }
    return textSize;
  }
}
