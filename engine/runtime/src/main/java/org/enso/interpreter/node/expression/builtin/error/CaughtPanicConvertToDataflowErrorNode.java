package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Caught_Panic",
    name = "convert_to_dataflow_error",
    description = "Converts a Caught_Panic into a Dataflow Error")
public abstract class CaughtPanicConvertToDataflowErrorNode extends Node {
  static CaughtPanicConvertToDataflowErrorNode build() {
    return CaughtPanicConvertToDataflowErrorNodeGen.create();
  }

  abstract Stateful execute(@MonadicState Object state, Atom _this);

  @Specialization
  Stateful doExecute(
      @MonadicState Object state,
      Atom _this,
      @CachedLibrary(limit = "5") InteropLibrary interopLibrary) {
    Builtins builtins = Context.get(this).getBuiltins();
    Object payload = _this.getFields()[0];
    Object originalException = _this.getFields()[1];
    if (interopLibrary.isException(originalException)) {
      return new Stateful(
          state, DataflowError.withTrace(payload, (AbstractTruffleException) originalException));
    } else {
      throw new PanicException(
          builtins
              .error()
              .makeTypeError("Exception", originalException, "internal_original_exception"),
          this);
    }
  }
}
