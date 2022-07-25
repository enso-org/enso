package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

import java.nio.ByteBuffer;

@ExportLibrary(InteropLibrary.class)
@Builtin(pkg = "mutable", stdlibName = "Standard.Base.Data.Array.Array_Over_Buffer")
public class ArrayOverBuffer implements TruffleObject {
  private final ByteBuffer buffer;

  private ArrayOverBuffer(ByteBuffer buffer) {
    this.buffer = buffer;
  }

  @ExportMessage
  Object readArrayElement(long index) {
    return (long) buffer.get(Math.toIntExact(index));
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
}
