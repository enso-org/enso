package org.enso.interpreter.runtime.callable.atom.unboxing;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;

import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.state.State;

/**
 * Getter node that reads a field value. If the value is a thunk the node
 * evaluates it and replaces the original lazy value with the new value.
 */
final class SuspendedFieldGetterNode extends UnboxingAtom.FieldGetterNode {
  @Node.Child
  private UnboxingAtom.FieldSetterNode set;
  @Node.Child
  private UnboxingAtom.FieldGetterNode get;
  @Node.Child
  private InvokeFunctionNode invoke = InvokeFunctionNode.build(
          new CallArgumentInfo[0], InvokeCallableNode.DefaultsExecutionMode.EXECUTE, InvokeCallableNode.ArgumentsExecutionMode.EXECUTE
  );

  private SuspendedFieldGetterNode(UnboxingAtom.FieldGetterNode get, UnboxingAtom.FieldSetterNode set) {
    this.get = get;
    this.set = set;
  }

  static UnboxingAtom.FieldGetterNode build(UnboxingAtom.FieldGetterNode get, UnboxingAtom.FieldSetterNode set) {
    return new SuspendedFieldGetterNode(get, set);
  }

  @Override
  public Object execute(Atom atom) {
    java.lang.Object value = get.execute(atom);
    if (value instanceof Function fn && fn.isThunk()) {
      try {
        org.enso.interpreter.runtime.EnsoContext ctx = EnsoContext.get(this);
        java.lang.Object newValue = invoke.execute(fn, null, State.create(ctx), new Object[0]);
        set.execute(atom, newValue);
        return newValue;
      } catch (AbstractTruffleException ex) {
        var rethrow = new SuspendedException(ex);
        set.execute(atom, rethrow);
        throw ex;
      }
    } else if (value instanceof SuspendedException suspended) {
      throw suspended.ex;
    } else {
      return value;
    }
  }

  private static final class SuspendedException implements EnsoObject {
    final AbstractTruffleException ex;

    SuspendedException(AbstractTruffleException ex) {
      this.ex = ex;
    }
  }
}
