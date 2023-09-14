package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

abstract class FloatNode extends Node {
  static final String INTEROP_LIMIT = "3";

  final boolean isForeignNumber(InteropLibrary iop, TruffleObject obj) {
    if (obj instanceof EnsoBigInteger) {
      return false;
    }
    return iop.isNumber(obj);
  }

  final Object handleInterop(
      boolean incomparableError,
      double self,
      TruffleObject that,
      InteropLibrary iop,
      ToEnsoNumberNode toEnsoNumberNode) {
    try {
      if (iop.fitsInLong(that)) {
        return iop.asLong(that);
      } else if (iop.fitsInDouble(that)) {
        return iop.asDouble(that);
      } else if (iop.fitsInBigInteger(that)) {
        return toEnsoNumberNode.execute(iop.asBigInteger(that));
      }
    } catch (UnsupportedMessageException ex) {
    }
    return incomparableError ? incomparableError(self, that) : panicOtherwise(self, that);
  }

  final PanicException panicOtherwise(double self, Object that) {
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
