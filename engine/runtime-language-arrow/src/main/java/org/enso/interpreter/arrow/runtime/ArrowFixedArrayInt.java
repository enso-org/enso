package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.arrow.LogicalLayout;

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedArrayInt implements TruffleObject {
  private final int size;
  private final ByteBufferDirect buffer;
  private final LogicalLayout unit;

  public ArrowFixedArrayInt(ByteBufferDirect buffer, int size, LogicalLayout unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = buffer;
  }

  public LogicalLayout getUnit() {
    return unit;
  }

  @ExportMessage
  public boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  @ImportStatic(LogicalLayout.class)
  static class ReadArrayElement {
    @Specialization(guards = "receiver.getUnit() == Int8")
    public static Object doByte(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at = adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.get(at);
    }

    @Specialization(guards = "receiver.getUnit() == Int16")
    public static Object doShort(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at = adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.getShort(at);
    }

    @Specialization(guards = "receiver.getUnit() == Int32")
    public static Object doInt(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at = adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.getInt(at);
    }

    @Specialization(guards = "receiver.getUnit() == Int64")
    public static Object doLong(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at = adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.getLong(at);
    }
  }

  public static int adjustedIndex(ByteBufferDirect buffer, LogicalLayout unit, int size, long index)
      throws InvalidArrayIndexException {
    if (index >= size || index < 0) {
      throw InvalidArrayIndexException.create(index);
    }
    return typeAdjustedIndex(index, unit);
  }

  @ExportMessage
  long getArraySize() {
    return size;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index >= 0 && index < size && !buffer.isNull((int) index);
  }

  private static int typeAdjustedIndex(long index, SizeInBytes unit) {
    return Math.toIntExact(index * unit.sizeInBytes());
  }
}
