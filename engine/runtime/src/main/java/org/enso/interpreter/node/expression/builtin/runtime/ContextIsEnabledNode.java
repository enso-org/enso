package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.ExecutionEnvironment;
import org.enso.interpreter.runtime.state.HasContextEnabledNode;

@BuiltinMethod(
    type = "Context",
    name = "is_enabled_builtin",
    description = "Check if the context is enabled in the provided execution environment.")
public class ContextIsEnabledNode extends Node {
  private @Child ExpectStringNode expectStringNode = ExpectStringNode.build();
  private @Child HasContextEnabledNode hasContextEnabledNode = HasContextEnabledNode.create();

  Object execute(Atom self, Object environmentName) {
    String envName = expectStringNode.execute(environmentName);
    ExecutionEnvironment currentEnv = EnsoContext.get(this).getExecutionEnvironment();
    if (!currentEnv.getName().equals(envName)) {
      Atom error =
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .makeUnimplemented("execution environment mismatch");
      throw new PanicException(error, this);
    }
    return hasContextEnabledNode.executeHasContextEnabled(currentEnv, self.getConstructor());
  }
}
