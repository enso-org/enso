package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import java.util.List;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.typecheck.TypeCheckValueNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.atom.UnboxingAtom.FieldGetterNode;
import org.enso.interpreter.runtime.data.atom.UnboxingAtom.FieldSetterNode;
import org.enso.interpreter.runtime.error.DataflowError;

/**
 * Getter node that reads a field value. If the value is a thunk the node evaluates it and replaces
 * the original lazy value with the new value.
 */
final class SuspendedFieldGetterNode extends UnboxingAtom.FieldGetterNode {
  @Node.Child private UnboxingAtom.FieldSetterNode set;
  @Node.Child private UnboxingAtom.FieldGetterNode get;
  private final BranchProfile exceptionalState = BranchProfile.create();

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
    return fn.isThunk() || TypeCheckValueNode.isWrappedThunk(fn);
  }

  @Override
  public Object execute(Atom atom) {
    java.lang.Object value = get.execute(atom);
    if (value instanceof Function fn && shallBeExtracted(fn)) {
      try {
        var state = Function.ArgumentsHelper.getState(fn.getScope().getArguments());
        var newValue = invoke.execute(fn, null, state, new Object[0]);
        set.execute(atom, newValue);
        return newValue;
      } catch (AbstractTruffleException ex) {
        exceptionalState.enter();
        var rethrow = DataflowError.withTrace(ex, ex);
        set.execute(atom, rethrow);
        throw ex;
      }
    } else if (value instanceof DataflowError suspended) {
      exceptionalState.enter();
      if (suspended.getPayload() instanceof AbstractTruffleException ex) {
        throw ex;
      }
    }
    return value;
  }
}
