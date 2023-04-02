package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(type = "Context", name = "name", description = "Name of the context.")
public class ContextName extends Node {
  Object execute(Atom self) {
    return Text.create(self.getConstructor().getName());
  }
}
