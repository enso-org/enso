package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;

import java.io.PrintWriter;
import java.io.StringWriter;

public class DisplayArrayUtils {

  @CompilerDirectives.TruffleBoundary
  public static String toDisplayString(
      Object arrayLike, boolean allowSideEffects, InteropLibrary iop) {
    StringBuilder sb = new StringBuilder();
    try {
      sb.append('[');
      String sep = "";
      long len = iop.getArraySize(arrayLike);
      for (long i = 0; i < len; i++) {
        sb.append(sep);

        Object at = iop.readArrayElement(arrayLike, i);
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
      sb.append("...\n").append(w);
    }
    return sb.toString();
  }

  private static Object showObject(InteropLibrary iop, boolean allowSideEffects, Object child)
      throws UnsupportedMessageException {
    if (child == null) {
      return "null";
    } else if (child instanceof Boolean) {
      return (boolean) child ? "True" : "False";
    } else {
      return iop.toDisplayString(child, allowSideEffects);
    }
  }
}
