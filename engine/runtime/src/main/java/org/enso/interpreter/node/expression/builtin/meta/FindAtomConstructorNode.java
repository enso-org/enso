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
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Meta",
    name = "find_atom_constructor",
    description = "Checks if the argument is a constructor.",
    autoRegister = false)
final class FindAtomConstructorNode extends Node {
  Object execute(VirtualFrame frame, @Suspend @AcceptsError Object value) {
    return findAtomConstructor(this, value, frame);
  }

  /**
   * "Enso facing" method to turn a value into {@link AtomConstructor} for purposes of <em>meta
   * programming API</em>. Executes "common logic" for finding the constructor for provided value
   * (which is "identity" if the value already is an {@link AtomConstructor}). Then it performs
   * <em>encapsulation related accessiblity checks</em> and returns an <em>interop value</em> that
   * can flow via Enso interpreter freely.
   *
   * @param who the node performing the query
   * @param value value to check
   * @param frame the frame to further evaluate {@code value} at if necessary, the value can be
   *     {@code null} especially when the {@code value} is known to be {@link AtomConstructor}
   *     already
   * @return either {@link AtomConstructor} on success, or {@code Nothing} when the {@code value}
   *     doesn't represent a constructor, or a [@link DataflowError} with {@code Private_Access}
   *     failure when the value is a constructor, but it is not accessible due to encapsulation
   *     rules
   */
  static Object findAtomConstructor(Node who, Object value, VirtualFrame frame) {
    var ctx = EnsoContext.get(who);
    var ac = findConstructorOrNull(ctx, value, frame);
    if (ac != null) {
      if (ac.getType().hasAllConstructorsPrivate()) {
        var errors = ctx.getBuiltins().error();
        var err =
            errors.makePrivateAccessError(
                null, null, "constructor", "Cannot access private constructors.");
        return DataflowError.withDefaultTrace(err, who);
      } else {
        return ac;
      }
    } else {
      return ctx.getNothing();
    }
  }

  private static AtomConstructor findConstructorOrNull(
      EnsoContext ctx, Object value, VirtualFrame frame) {
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
          var state = ctx.currentState();
          var thunkSolver = ThunkExecutorNode.getUncached();
          value = thunkSolver.executeThunk(frame, value, state, BaseNode.TailStatus.NOT_TAIL);
          continue;
        }
      }
      return null;
    }
  }
}
