package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.PanicException;

final class ArrayPanics {
  private ArrayPanics() {}

  static PanicException notAnArrayPanic(Node node, Object self) {
    var ctx = EnsoContext.get(node);
    var err =
        ctx.getBuiltins()
            .error()
            .makeUnsupportedArgumentsError(new Object[] {self}, "Not an array");
    return new PanicException(err, node);
  }

  static PanicException unsupportedArgument(Node node, Object arg, String msg) {
    var ctx = EnsoContext.get(node);
    var err = ctx.getBuiltins().error().makeUnsupportedArgumentsError(new Object[] {arg}, msg);
    return new PanicException(err, node);
  }

  static PanicException indexOutOfBounds(Node node, int index, int size) {
    var ctx = EnsoContext.get(node);
    var err = ctx.getBuiltins().error().makeIndexOutOfBounds(index, size);
    throw new PanicException(err, node);
  }

  static PanicException invalidIndex(Node node, Object src, InvalidArrayIndexException e) {
    var ctx = EnsoContext.get(node);
    var err = ctx.getBuiltins().error().makeInvalidArrayIndex(src, e.getInvalidIndex());
    return new PanicException(err, node);
  }

  static PanicException typeError(Node node, Object src, String msg) {
    var builtins = EnsoContext.get(node).getBuiltins();
    var err = builtins.error().makeTypeError(builtins.array(), src, msg);
    return new PanicException(err, node);
  }
}
