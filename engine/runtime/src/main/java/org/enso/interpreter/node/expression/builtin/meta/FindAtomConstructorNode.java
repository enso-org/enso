package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Meta",
    name = "find_atom_constructor",
    description = "Checks if the argument is a constructor.",
    autoRegister = false)
final class FindAtomConstructorNode extends Node {
  EnsoObject execute(VirtualFrame frame, @Suspend @AcceptsError Object value, State state) {
    var ac = findConstructor(value, frame, state);
    if (ac != null) {
      return ac;
    } else {
      var ctx = EnsoContext.get(this);
      return ctx.getNothing();
    }
  }

  static AtomConstructor findConstructor(Object value, VirtualFrame frame, State state) {
    for (; ; ) {
      if (value instanceof AtomConstructor atom) {
        return atom;
      }
      if (value instanceof Function fn) {
        if (AtomConstructor.accessorFor(fn) instanceof AtomConstructor atom) {
          return atom;
        }
        if (MethodRootNode.constructorFor(fn) instanceof AtomConstructor atom) {
          return atom;
        }
        if (fn.isThunk()) {
          var thunkSolver = ThunkExecutorNode.getUncached();
          value = thunkSolver.executeThunk(frame, value, state, BaseNode.TailStatus.NOT_TAIL);
          continue;
        }
      }
      return null;
    }
  }
}
