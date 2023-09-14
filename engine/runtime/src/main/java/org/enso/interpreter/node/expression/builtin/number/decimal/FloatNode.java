package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;

abstract class FloatNode extends Node {
  final double handleOther(double self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }

  final DataflowError incomparableError(Object self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var incomparableErr = builtins.error().makeIncomparableValues(self, that);
    return DataflowError.withoutTrace(incomparableErr, this);
  }
}
