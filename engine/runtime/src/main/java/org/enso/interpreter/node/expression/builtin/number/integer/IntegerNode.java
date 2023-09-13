package org.enso.interpreter.node.expression.builtin.number.integer;

import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.Node;

abstract class IntegerNode extends Node {
  IntegerNode() {}

  @TruffleBoundary
  final PanicException throwTypeErrorIfNotInt(Object self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var intType = builtins.number().getInteger();
    var selfType = TypesLibrary.getUncached().getType(self);
    if (selfType != intType) {
      return new PanicException(builtins.error().makeTypeError(intType, self, "self"), this);
    } else {
      return new PanicException(builtins.error().makeTypeError(intType, that, "that"), this);
    }
  }

  @TruffleBoundary
  final PanicException throwTypeErrorIfNotInt(Object self) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var intType = builtins.number().getInteger();
    return new PanicException(builtins.error().makeTypeError(intType, self, "self"), this);
  }

  final boolean isForeignNumber(InteropLibrary iop, TruffleObject obj) {
    if (obj instanceof EnsoBigInteger) {
      return false;
    }
    return iop.isNumber(obj);
  }
}
