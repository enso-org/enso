package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.nio.ByteBuffer;

@ExportLibrary(InteropLibrary.class)
public final class ArrayOverBuffer implements TruffleObject {
  private final ByteBuffer buffer;

  private ArrayOverBuffer(ByteBuffer buffer) {
    this.buffer = buffer;
  }

  @ExportMessage
  Object readArrayElement(long index) throws InvalidArrayIndexException {
    try {
      return (long) buffer.get(buffer.position() + Math.toIntExact(index));
    } catch (IndexOutOfBoundsException e) {
      throw InvalidArrayIndexException.create(index);
    }
  }

  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index >= 0 && index < getArraySize();
  }

  @ExportMessage
  long getArraySize() {
    return buffer.remaining();
  }

  public static ArrayOverBuffer wrapBuffer(ByteBuffer buffer) {
    return new ArrayOverBuffer(buffer);
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
    final InteropLibrary iop = InteropLibrary.getUncached();
    return DisplayArrayUtils.toDisplayString(this, allowSideEffects, iop);
  }
}
