package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.runtime.GetStackTraceNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Panic",
    name = "primitive_get_attached_stack_trace",
    description = "Gets the stack trace attached to the throwable.")
public class GetAttachedStackTraceNode extends Node {
  @Child private InteropLibrary iop = InteropLibrary.getFactory().createDispatched(3);

  Object execute(@AcceptsError Object error) {
    try {
      if (error instanceof DataflowError) {
        var arr = iop.getExceptionStackTrace(error);
        assert InteropLibrary.getUncached().hasArrayElements(arr);
        var len = iop.getArraySize(arr);
        var count = 0;
        for (long i = 0; i < len; i++) {
          try {
            var elem = iop.readArrayElement(arr, i);
            if (includeFrame(iop, elem)) {
              count++;
            }
          } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
          }
        }
        var res = new Object[count];
        var at = 0;
        for (long i = 0; i < len; i++) {
          try {
            var elem = iop.readArrayElement(arr, i);
            if (includeFrame(iop, elem)) {
              res[at++] = elem;
            }
          } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
          }
        }
        return ArrayLikeHelpers.wrapObjectsWithCheckAt(res);
      }
    } catch (UnsupportedMessageException ex) {
      // OK, go on
    }
    if (error instanceof Throwable) {
      return GetStackTraceNode.stackTraceToArray((Throwable) error);
    } else {
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeTypeError("Throwable", error, "throwable"), this);
    }
  }

  private boolean includeFrame(InteropLibrary iop, Object elem) throws UnsupportedMessageException {
    if (!iop.hasSourceLocation(elem)) {
      return false;
    }
    var ss = iop.getSourceLocation(elem);
    if (ss == null) {
      return false;
    }
    var src = ss.getSource();
    return src != null && !src.isInternal();
  }
}
