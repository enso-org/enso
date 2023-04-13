package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.node.expression.builtin.bool.IfThenElseNode;
import org.enso.interpreter.node.expression.builtin.bool.IfThenNode;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * This node is responsible for optimising function calls.
 *
 * <p>Where possible, it will make the call as a direct call, with potential for inlining.
 */
@NodeInfo(shortName = "ExecCall", description = "Optimises function calls")
@GenerateUncached
public abstract class ExecuteCallNode extends Node {

  ExecuteCallNode() {}

  /**
   * Creates an instance of this node.
   *
   * @return an instance of this node
   */
  public static ExecuteCallNode build() {
    return ExecuteCallNodeGen.create();
  }

  /**
   * Calls the function directly.
   *
   * <p>This specialisation comes into play where the call target for the provided function is
   * already cached. THis means that the call can be made quickly.
   *
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the current state value
   * @param arguments the arguments passed to {@code function} in the expected positional order
   * @param cachedTarget the cached call target for {@code function}
   * @param callNode the cached call node for {@code cachedTarget}
   * @return the result of executing {@code function} on {@code arguments}
   */
  @Specialization(guards = "function.getCallTarget() == cachedTarget")
  protected Object callDirect(
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      @Cached("function.getCallTarget()") RootCallTarget cachedTarget,
      @Cached("createWithPossibleClone(cachedTarget, state)") DirectCallNode callNode) {
    return callNode.call(
        Function.ArgumentsHelper.buildArguments(function, callerInfo, state, arguments));
  }

  static DirectCallNode createWithPossibleClone(RootCallTarget t, Object state) {
    if (t.getRootNode() instanceof BuiltinRootNode n) {
      if (n.getName().contains("if_then")) {
        if (n.cloneBuiltin().getChildren().iterator().next() instanceof IfThenElseNode ite) {
          class IteDirectCallNode extends DirectCallNode {
            public IteDirectCallNode() {
              super(null);
            }

            @Override
            public Object call(Object... arguments) {
              State state = Function.ArgumentsHelper.getState(arguments);
              Object[] arg = Function.ArgumentsHelper.getPositionalArguments(arguments);
              var self = TypesGen.asBoolean(arg[0]);
              var if_true = arg[1];
              var if_false = arg[2];
              return ite.execute(state, self, if_true, if_false);
            }

            @Override
            public boolean isInlinable() {
              return true;
            }

            @Override
            public boolean isInliningForced() {
              return true;
            }

            @Override
            public void forceInlining() {
            }

            @Override
            public boolean isCallTargetCloningAllowed() {
              return false;
            }

            @Override
            public boolean cloneCallTarget() {
              return false;
            }

            @Override
            public CallTarget getClonedCallTarget() {
              return null;
            }
          }
          return new IteDirectCallNode();
        }
      }
    }
    return DirectCallNode.create(t);
  }

  /**
   * Calls the function with a lookup.
   *
   * <p>This specialisation is used in the case where there is no cached call target for the
   * provided function. This is much slower and should, in general, be avoided.
   *
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the current state value
   * @param arguments the arguments passed to {@code function} in the expected positional order
   * @param callNode the cached call node for making indirect calls
   * @return the result of executing {@code function} on {@code arguments}
   */
  @Specialization(replaces = "callDirect")
  protected Object callIndirect(
      Function function,
      CallerInfo callerInfo,
      Object state,
      Object[] arguments,
      @Cached IndirectCallNode callNode) {
    return callNode.call(
        function.getCallTarget(),
        Function.ArgumentsHelper.buildArguments(function, callerInfo, state, arguments));
  }

  /**
   * Executes the function call.
   *
   * @param function the function to execute
   * @param callerInfo the caller info to pass to the function
   * @param state the state value to pass to the function
   * @param arguments the arguments to be passed to {@code function}
   * @return the result of executing {@code function} on {@code arguments}
   */
  public abstract Object executeCall(
      Function function, CallerInfo callerInfo, Object state, Object[] arguments);
}
