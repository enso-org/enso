package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(description = "Helper node to handle Bool values that can also return DataflowErrors")
public abstract class ToBoolNode extends Node {

  public static ToBoolNode build() {
    return ToBoolNodeGen.create();
  }

  abstract Stateful execute(@MonadicState Object state, Object value);

  @Specialization
  Stateful doBool(@MonadicState Object state, boolean value) {
    return new Stateful(state, value);
  }

  @Specialization
  Stateful doDataflowError(@MonadicState Object state, DataflowError value) {
    return new Stateful(state, value);
  }

  @Fallback
  Stateful doPanic(@MonadicState Object state, Object value) {
    var typeError = Context.get(this).getBuiltins().error().makeTypeError("Boolean", value, "bool");
    throw new PanicException(typeError, this);
  }
}
