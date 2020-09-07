package org.enso.interpreter.node.expression.builtin.array;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(type = "Array", name = "empty", description = "Creates an empty array.")
public class EmptyNode extends Node {

  Object execute(Object _this) {
    return new Vector();
  }
}
