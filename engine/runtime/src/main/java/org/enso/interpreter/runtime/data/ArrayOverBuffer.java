package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.node.expression.builtin.error.InvalidArrayIndex;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.error.WarningsLibrary;

import java.io.PrintWriter;
import java.io.StringWriter;
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
    StringBuilder sb = new StringBuilder();
    try {
      sb.append('[');
      String sep = "";
      long len = getArraySize();
      for (long i = 0; i < len; i++) {
        sb.append(sep);

        Object at = readArrayElement(i);
        Object str = showObject(iop, allowSideEffects, at);
        if (iop.isString(str)) {
          sb.append(iop.asString(str));
        } else {
          sb.append("_");
        }
        sep = ", ";
      }
      sb.append(']');
    } catch (InvalidArrayIndexException | UnsupportedMessageException ex) {
      StringWriter w = new StringWriter();
      ex.printStackTrace(new PrintWriter(w));
      sb.append("...\n").append(w.toString());
    }
    return sb.toString();
  }

  @CompilerDirectives.TruffleBoundary
  private Object showObject(InteropLibrary iop, boolean allowSideEffects, Object child)
      throws UnsupportedMessageException {
    if (child == null) {
      return "null";
    } else if (child instanceof Boolean) {
      return (boolean) child ? "True" : "False";
    } else {
      return iop.toDisplayString(child, allowSideEffects);
    }
  }

  public byte[] backingArray() {
    return buffer.array();
  }
}
