package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Caught_Panic",
    name = "convert_to_dataflow_error",
    description = "Converts a Caught_Panic into a Dataflow Error")
public abstract class CaughtPanicConvertToDataflowErrorNode extends Node {
  static CaughtPanicConvertToDataflowErrorNode build() {
    return CaughtPanicConvertToDataflowErrorNodeGen.create();
  }

  abstract Object execute(State state, Atom self);

  @Specialization
  Object doExecute(
      State state, Atom self, @CachedLibrary(limit = "5") InteropLibrary interopLibrary) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    Object payload = self.getFields()[0];
    Object originalException = self.getFields()[1];
    if (interopLibrary.isException(originalException)) {
      return DataflowError.withTrace(payload, (AbstractTruffleException) originalException);
    } else {
      throw new PanicException(
          builtins
              .error()
              .makeTypeError("Exception", originalException, "internal_original_exception"),
          this);
    }
  }
}
