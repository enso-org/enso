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

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedArrayInt implements TruffleObject {
  private final int size;
  private final ByteBufferDirect buffer;

  private final IntUnit unit;

  public ArrowFixedArrayInt(int size, IntUnit unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = allocateBuffer(size * this.unit.sizeInBytes(), size);
  }

  public ArrowFixedArrayInt(ByteBufferDirect buffer, IntUnit unit)
      throws UnsupportedMessageException {
    this.size = buffer.capacity() / unit.sizeInBytes();
    this.unit = unit;
    this.buffer = buffer;
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
    public static Object doByte(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException {
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      var at = typeAdjustedIndex(index, receiver.unit);
      return receiver.buffer.get(at);
    }

    @Specialization(guards = "receiver.getUnit() == Byte2")
    public static Object doShort(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException {
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      var at = typeAdjustedIndex(index, receiver.unit);
      return receiver.buffer.getShort(at);
    }

    @Specialization(guards = "receiver.getUnit() == Byte4")
    public static Object doInt(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException {

      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      var at = typeAdjustedIndex(index, receiver.unit);
      return receiver.buffer.getInt(at);
    }

    @Specialization(guards = "receiver.getUnit() == Byte8")
    public static Object doLong(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException {
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      var at = typeAdjustedIndex(index, receiver.unit);
      return receiver.buffer.getLong(at);
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
      if (!iop.fitsInByte(value)) {
        throw UnsupportedMessageException.create();
      }
      receiver.buffer.put(typeAdjustedIndex(index, receiver.unit), (iop.asByte(value)));
    }

    @Specialization(guards = "receiver.getUnit() == Byte2")
    public static void doShort(
        ArrowFixedArrayInt receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      if (!iop.fitsInShort(value)) {
        throw UnsupportedMessageException.create();
      }
      receiver.buffer.putShort(typeAdjustedIndex(index, receiver.unit), (iop.asShort(value)));
    }

    @Specialization(guards = "receiver.getUnit() == Byte4")
    public static void doInt(
        ArrowFixedArrayInt receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      if (!iop.fitsInInt(value)) {
        throw UnsupportedMessageException.create();
      }
      receiver.buffer.putInt(typeAdjustedIndex(index, receiver.unit), (iop.asInt(value)));
    }

    @Specialization(guards = "receiver.getUnit() == Byte8")
    public static void doLong(
        ArrowFixedArrayInt receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      if (!iop.fitsInLong(value)) {
        throw UnsupportedMessageException.create();
      }
      receiver.buffer.putLong(typeAdjustedIndex(index, receiver.unit), (iop.asLong(value)));
    }
  }

  public enum IntUnit implements SizeInBytes {
    Byte1(8),
    Byte2(16),
    Byte4(32),
    Byte8(64);

    private final int bits;

    IntUnit(int bits) {
      this.bits = bits;
    }

    public int sizeInBytes() {
      return bits / 8;
    }
  }

  @ExportMessage
  long getArraySize() {
    return size;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index >= 0 && index < size && !buffer.isNull((int) index);
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return index >= 0 && index < size;
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    return index >= 0 && index < size;
  }

  @CompilerDirectives.TruffleBoundary
  private ByteBufferDirect allocateBuffer(int sizeInBytes, int size) {
    return new ByteBufferDirect(sizeInBytes, size);
  }

  private static int typeAdjustedIndex(long index, SizeInBytes unit) {
    return Math.toIntExact(index * unit.sizeInBytes());
  }
}
