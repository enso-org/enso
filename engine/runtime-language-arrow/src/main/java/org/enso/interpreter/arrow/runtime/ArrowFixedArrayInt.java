package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.nio.ByteBuffer;

@ExportLibrary(InteropLibrary.class)
public class ArrowFixedArrayInt implements TruffleObject {
  private final long size;
  private final ByteBuffer buffer;

  private final IntUnit unit;

  public ArrowFixedArrayInt(long size, IntUnit unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = allocateBuffer((int) size * this.unit.sizeInBytes());
  }

  public IntUnit getUnit() {
    return unit;
  }

  @ExportMessage
  public boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  @ImportStatic(IntUnit.class)
  static class ReadArrayElement {
    @Specialization(guards = "receiver.getUnit() == Byte1")
    public static Object doByte(ArrowFixedArrayInt receiver, long index) {
      return receiver.buffer.get((int) index);
    }

    @Specialization(guards = "receiver.getUnit() == Byte2")
    public static Object doShort(ArrowFixedArrayInt receiver, long index) {
      return receiver.buffer.getShort((int) index);
    }

    @Specialization(guards = "receiver.getUnit() == Byte4")
    public static Object doInt(ArrowFixedArrayInt receiver, long index) {
      return receiver.buffer.getInt((int) index);
    }

    @Specialization(guards = "receiver.getUnit() == Byte8")
    public static Object doLong(ArrowFixedArrayInt receiver, long index) {
      return receiver.buffer.getLong((int) index);
    }
  }

  @ExportMessage
  @ImportStatic(IntUnit.class)
  static class WriteArrayElement {
    @Specialization(guards = "receiver.getUnit() == Byte1")
    public static void doByte(
        ArrowFixedArrayInt receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      assert (iop.fitsInByte(value));
      receiver.buffer.put((int) index, (iop.asByte(value)));
    }

    @Specialization(guards = "receiver.getUnit() == Byte2")
    public static void doShort(
        ArrowFixedArrayInt receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      assert (iop.fitsInShort(value));
      receiver.buffer.putShort((int) index, (iop.asShort(value)));
    }

    @Specialization(guards = "receiver.getUnit() == Byte4")
    public static void doInt(
        ArrowFixedArrayInt receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      assert (iop.fitsInInt(value));
      receiver.buffer.putInt((int) index, (iop.asInt(value)));
    }

    @Specialization(guards = "receiver.getUnit() == Byte8")
    public static void doLong(
        ArrowFixedArrayInt receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      assert (iop.fitsInLong(value));
      receiver.buffer.putLong((int) index, (iop.asLong(value)));
    }
  }

  public enum IntUnit {
    Byte1(8),
    Byte2(16),
    Byte4(32),
    Byte8(64);

    private final int bits;

    IntUnit(int bits) {
      this.bits = bits;
    }

    int sizeInBytes() {
      return bits / 8;
    }
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

  @CompilerDirectives.TruffleBoundary
  private ByteBuffer allocateBuffer(int size) {
    return ByteBuffer.allocate(size);
  }
}
