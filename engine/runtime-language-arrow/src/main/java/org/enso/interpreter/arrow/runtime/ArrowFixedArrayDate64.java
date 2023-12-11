package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.nio.ByteBuffer;
import java.time.LocalDate;
import java.time.ZoneOffset;

@ExportLibrary(InteropLibrary.class)
public class ArrowFixedArrayDate64 implements TruffleObject {
  private final long size;
  private final ByteBuffer buffer;
  private static final int elementSize = 8;

  public ArrowFixedArrayDate64(long size) {
    this.size = size;
    this.buffer = ByteBuffer.allocate((int) size * elementSize);
  }

  @ExportMessage
  public boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  public Object readArrayElement(long index) {
    // TODO: Needs null bitmap
    var daysSinceEpoch = buffer.getInt((int) index * elementSize);
    var localDate = LocalDate.ofEpochDay(daysSinceEpoch);
    return new ArrowDate(localDate);
  }

  @ExportMessage
  public void writeArrayElement(
      long index, Object value, @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedMessageException {
    assert iop.isDate(value);

    var at = index * elementSize;
    if (iop.isTimeZone(value)) {
      var zone = iop.asTimeZone(value);
      var localDate = iop.asDate(value);
      var milisecondsSinceEpoch = localDate.atStartOfDay(zone).toInstant().toEpochMilli();
      buffer.putLong((int) at, milisecondsSinceEpoch);
    } else {
      var localDate = iop.asDate(value);
      var milisecondsSinceEpoch = localDate.atStartOfDay(ZoneOffset.UTC).toInstant().toEpochMilli();
      buffer.putLong((int) at, milisecondsSinceEpoch);
    }
    // TODO: Update nulls bitmap
  }

  @ExportMessage
  final long getArraySize() {
    return size;
  }

  @ExportMessage
  final boolean isArrayElementReadable(long index) {
    return index >= 0 && index < size;
  }

  @ExportMessage
  final boolean isArrayElementModifiable(long index) {
    return index >= 0 && index < size;
  }

  @ExportMessage
  final boolean isArrayElementInsertable(long index) {
    return index >= 0 && index < size;
  }
}
