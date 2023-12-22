package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.TruffleStackTraceElement;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import java.util.List;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Runtime",
    name = "primitive_get_stack_trace",
    description = "Gets the current execution stacktrace.",
    autoRegister = false)
public class GetStackTraceNode extends Node {

  @NeverDefault
  public static GetStackTraceNode create() {
    return new GetStackTraceNode();
  }

  @CompilerDirectives.TruffleBoundary
  private static EnsoObject wrapStackTraceElements(List<TruffleStackTraceElement> elements) {
    var arr = new Object[elements.size()];
    for (var i = 0; i < arr.length; i++) {
      var e = elements.get(i);
      arr[i] = e.getGuestObject();
    }
    var vector = ArrayLikeHelpers.asVectorWithCheckAt(arr);
    try {
      return filterStackTraceVector(InteropLibrary.getUncached(), vector);
    } catch (UnsupportedMessageException ex) {
      assert raise(RuntimeException.class, ex);
      return ArrayLikeHelpers.empty();
    }
  }

  private static boolean includeFrame(InteropLibrary iop, Object elem)
      throws UnsupportedMessageException {
    if (!iop.hasSourceLocation(elem)) {
      return false;
    }
    var ss = iop.getSourceLocation(elem);
    if (ss == null) {
      return false;
    }
    var src = ss.getSource();
    return src != null && src.hasCharacters() && !src.isInternal();
  }

  private static EnsoObject filterStackTraceVector(InteropLibrary iop, Object elements)
      throws UnsupportedMessageException {
    var size = iop.getArraySize(elements);
    var count = 0;
    for (long i = 0; i < size; i++) {
      try {
        var element = iop.readArrayElement(elements, i);
        if (includeFrame(iop, element)) {
          count++;
        }
      } catch (InvalidArrayIndexException ex) {
        assert raise(RuntimeException.class, ex);
      }
    }
    var arr = new Object[count];
    var at = 0;
    for (long i = 0; i < size; i++) {
      try {
        var element = iop.readArrayElement(elements, i);
        if (includeFrame(iop, element)) {
          arr[at++] = element;
        }
      } catch (InvalidArrayIndexException ex) {
        assert raise(RuntimeException.class, ex);
      }
    }
    return ArrayLikeHelpers.wrapObjectsWithCheckAt(arr);
  }

  EnsoObject execute(VirtualFrame requestOwnStackFrame) {
    var exception = new PanicException(Text.create("Stacktrace"), this);
    TruffleStackTrace.fillIn(exception);
    return stackTraceToArray(exception);
  }

  public static EnsoObject stackTraceToArray(InteropLibrary iop, Object exception) {
    if (iop.hasExceptionStackTrace(exception)) {
      try {
        var elements = iop.getExceptionStackTrace(exception);
        return filterStackTraceVector(iop, elements);
      } catch (UnsupportedMessageException ex) {
        assert raise(RuntimeException.class, ex);
        // return empty
      }
    } else if (exception instanceof Throwable t) {
      return stackTraceToArray(t);
    }
    return ArrayLikeHelpers.empty();
  }

  @CompilerDirectives.TruffleBoundary
  private static EnsoObject stackTraceToArray(Throwable exception) {
    var elements = TruffleStackTrace.getStackTrace(exception);
    if (elements == null) {
      return ArrayLikeHelpers.empty();
    }
    return wrapStackTraceElements(elements);
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> boolean raise(Class<E> type, Throwable t) throws E {
    throw (E) t;
  }
}
