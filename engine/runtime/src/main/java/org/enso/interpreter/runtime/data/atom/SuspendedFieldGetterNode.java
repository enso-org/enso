package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
import java.util.List;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.argument.ReadArgumentCheckNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.atom.UnboxingAtom.FieldGetterNode;
import org.enso.interpreter.runtime.data.atom.UnboxingAtom.FieldSetterNode;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.State;

/**
 * Getter node that reads a field value. If the value is a thunk the node evaluates it and replaces
 * the original lazy value with the new value.
 */
final class SuspendedFieldGetterNode extends UnboxingAtom.FieldGetterNode {
  @Node.Child private UnboxingAtom.FieldSetterNode set;
  @Node.Child private UnboxingAtom.FieldGetterNode get;

  @Node.Child
  private InvokeFunctionNode invoke =
      InvokeFunctionNode.build(
          new CallArgumentInfo[0],
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.EXECUTE);

  private SuspendedFieldGetterNode(
      UnboxingAtom.FieldGetterNode get, UnboxingAtom.FieldSetterNode set) {
    this.get = get;
    this.set = set;
  }

  static NodeFactory<? extends UnboxingAtom.FieldGetterNode> factory(
      NodeFactory<? extends FieldGetterNode> delegate,
      NodeFactory<? extends FieldSetterNode> setters) {
    class NF implements NodeFactory<UnboxingAtom.FieldGetterNode> {
      @Override
      public UnboxingAtom.FieldGetterNode createNode(Object... arguments) {
        var get = delegate.createNode(arguments);
        if (setters == null) {
          return get;
        } else {
          var set = setters.createNode(arguments);
          return build(get, set);
        }
      }

      @Override
      public Class<UnboxingAtom.FieldGetterNode> getNodeClass() {
        return UnboxingAtom.FieldGetterNode.class;
      }

      @Override
      public List<List<Class<?>>> getNodeSignatures() {
        return delegate.getNodeSignatures();
      }

      @Override
      public List<Class<? extends Node>> getExecutionSignature() {
        return delegate.getExecutionSignature();
      }

      @Override
      public FieldGetterNode getUncachedInstance() {
        if (setters == null) {
          return delegate.getUncachedInstance();
        } else {
          var set = setters.getUncachedInstance();
          var get = delegate.getUncachedInstance();
          return build(get, set);
        }
      }
    }
    return new NF();
  }

  private static UnboxingAtom.FieldGetterNode build(
      UnboxingAtom.FieldGetterNode get, UnboxingAtom.FieldSetterNode set) {
    return new SuspendedFieldGetterNode(get, set);
  }

  private static boolean shallBeExtracted(Function fn) {
    return fn.isThunk() || ReadArgumentCheckNode.isWrappedThunk(fn);
  }

  @Override
  public Object execute(Atom atom) {
    java.lang.Object value = get.execute(atom);
    if (value instanceof Function fn && shallBeExtracted(fn)) {
      try {
        org.enso.interpreter.runtime.EnsoContext ctx = EnsoContext.get(this);
        java.lang.Object newValue = invoke.execute(fn, null, State.create(ctx), new Object[0]);
        set.execute(atom, newValue);
        return newValue;
      } catch (AbstractTruffleException ex) {
        var rethrow = DataflowError.withTrace(ex, ex);
        set.execute(atom, rethrow);
        throw ex;
      }
    } else if (value instanceof DataflowError suspended
        && suspended.getPayload() instanceof AbstractTruffleException ex) {
      throw ex;
    } else {
      return value;
    }
  }
}
