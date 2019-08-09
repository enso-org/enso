package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.Arrays;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNode;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNodeGen;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.node.callable.dispatch.SimpleCallOptimiserNode;
import org.enso.interpreter.optimiser.tco.TailCallException;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.NotInvokableException;

/**
 * This node is responsible for organising callable calls so that they are ready to be made.
 *
 * <p>It handles computing the values of the arguments to the callable, and also the sorting of
 * those arguments into the correct positional order for the callable being called.
 */
@NodeInfo(shortName = "@", description = "Executes function")
@NodeChild(value = "callable", type = ExpressionNode.class)
public abstract class InvokeCallableNode extends ExpressionNode {
  @Children
  private @CompilationFinal(dimensions = 1) ExpressionNode[] argExpressions;

  @Child private ArgumentSorterNode argumentSorter;
  @Child private CallOptimiserNode callOptimiserNode;

  /**
   * Creates a new node for performing callable invocation.
   *
   * @param callArguments information on the arguments being passed to the {@link
   *     org.enso.interpreter.runtime.callable.Callable}
   */
  public InvokeCallableNode(CallArgument[] callArguments) {
    this.argExpressions =
        Arrays.stream(callArguments)
            .map(CallArgument::getExpression)
            .toArray(ExpressionNode[]::new);

    CallArgumentInfo[] argSchema =
        Arrays.stream(callArguments).map(CallArgumentInfo::new).toArray(CallArgumentInfo[]::new);

    this.callOptimiserNode = new SimpleCallOptimiserNode();
    this.argumentSorter = ArgumentSorterNodeGen.create(argSchema);
  }

  /**
   * Evaluates the arguments being passed to the callable.
   *
   * @param frame the stack frame in which to execute
   * @return the results of evaluating the function arguments
   */
  @ExplodeLoop
  public Object[] evaluateArguments(VirtualFrame frame) {
    Object[] computedArguments = new Object[this.argExpressions.length];

    for (int i = 0; i < this.argExpressions.length; ++i) {
      computedArguments[i] = this.argExpressions[i].executeGeneric(frame);
    }

    return computedArguments;
  }

  /**
   * Invokes a function directly on the arguments contained in this node.
   *
   * @param frame the stack frame in which to execute
   * @param callable the function to be executed
   * @return the result of executing {@code callable} on the known arguments
   */
  @Specialization
  public Object invokeFunction(VirtualFrame frame, Function callable) {
    Object[] evaluatedArguments = evaluateArguments(frame);
    Object[] sortedArguments = this.argumentSorter.execute(callable, evaluatedArguments);

    if (this.isTail()) {
      throw new TailCallException(callable, sortedArguments);
    } else {
      return this.callOptimiserNode.executeDispatch(callable, sortedArguments);
    }
  }

  /**
   * Invokes a constructor directly on the arguments contained in this node.
   *
   * @param frame the stack frame in which to execute
   * @param callable the constructor to be executed
   * @return the result of executing {@code callable} on the known arguments
   */
  @Specialization
  public Atom invokeConstructor(VirtualFrame frame, AtomConstructor callable) {
    Object[] evaluatedArguments = evaluateArguments(frame);
    Object[] sortedArguments = this.argumentSorter.execute(callable, evaluatedArguments);
    return callable.newInstance(sortedArguments);
  }

  /**
   * A fallback that should never be called.
   *
   * <p>If this is called, something has gone horribly wrong. It throws a {@link
   * NotInvokableException} to signal this.
   *
   * @param frame the stack frame in which to execute
   * @param callable the callable to be executed
   * @return error
   */
  @Fallback
  public Object invokeGeneric(VirtualFrame frame, Object callable) {
    throw new NotInvokableException(callable, this);
  }
}
