package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

public final class IntegerUtils {
  private IntegerUtils() {}

  @TruffleBoundary
  static PanicException throwTypeErrorIfNotInt(Object self, Object that, Node node) {
    var builtins = EnsoContext.get(node).getBuiltins();
    var intType = builtins.number().getInteger();
    var selfType = TypesLibrary.getUncached().getType(self);
    if (selfType != intType) {
      return new PanicException(builtins.error().makeTypeError(intType, self, "self"), node);
    } else {
      return new PanicException(builtins.error().makeTypeError(intType, that, "that"), node);
    }
  }

  @TruffleBoundary
  static PanicException throwTypeErrorIfNotInt(Object self, Node node) {
    var builtins = EnsoContext.get(node).getBuiltins();
    var intType = builtins.number().getInteger();
    return new PanicException(builtins.error().makeTypeError(intType, self, "self"), node);
  }
}
