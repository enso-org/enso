package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.state.ExecutionEnvironment;
import org.enso.interpreter.runtime.state.GetStateNode;
import org.enso.interpreter.runtime.state.HasContextEnabledNode;

@BuiltinMethod(
    type = "Context",
    name = "is_enabled_builtin",
    description = "Check if the context is enabled in the provided execution environment.")
final class ContextIsEnabledNode extends Node {
  private @Child GetStateNode stateNode = GetStateNode.create();
  private @Child HasContextEnabledNode hasContextEnabledNode = HasContextEnabledNode.create();

  final boolean execute(Object self, Atom context) {
    var currentEnv =
        stateNode.forClass(ExecutionEnvironment.class, EnsoContext::getGlobalExecutionEnvironment);
    var ret = hasContextEnabledNode.executeHasContextEnabled(currentEnv, context.getConstructor());
    return ret;
  }
}
