package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.warning.AppendWarningNode;
import org.enso.interpreter.runtime.warning.WarningsLibrary;

@BuiltinMethod(
    type = "Array_Like_Helpers",
    name = "length",
    description = "Returns the length of this Vector.")
public final class LengthVectorNode extends Node {
  @Child ArrayLikeLengthNode length = ArrayLikeLengthNode.create();
  @Child WarningsLibrary warn = WarningsLibrary.getFactory().createDispatched(3);
  @Child AppendWarningNode append;

  final Object execute(VirtualFrame frame, @AcceptsWarning Object arrayLike) {
    if (warn.hasWarnings(arrayLike)) {
      if (append == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        append = insert(AppendWarningNode.build());
      }
      EnsoHashMap extract;
      try {
        extract = warn.getWarnings(arrayLike, false);
      } catch (UnsupportedMessageException ex) {
        throw EnsoContext.get(this).raiseAssertionPanic(this, null, ex);
      }
      var len = length.executeLength(arrayLike);
      var joined = append.executeAppend(frame, len, extract);
      return joined;
    } else {
      return length.executeLength(arrayLike);
    }
  }
}
