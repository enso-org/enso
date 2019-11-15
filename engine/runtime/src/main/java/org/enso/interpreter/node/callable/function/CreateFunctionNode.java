package org.enso.interpreter.node.callable.function;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;

/**
 * This node is responsible for representing the definition of a function. It contains information
 * about the function's arguments, as well as the target for calling said function.
 */
public class CreateFunctionNode extends ExpressionNode {
  private final RootCallTarget callTarget;
  private final FunctionSchema schema;

  /**
   * Creates a new node to represent a function definition.
   *
   * @param callTarget the target for calling the function represented by this node
   * @param args information on the arguments to the function
   */
  public CreateFunctionNode(RootCallTarget callTarget, ArgumentDefinition[] args) {
    this.callTarget = callTarget;
    this.schema = new FunctionSchema(FunctionSchema.CallStrategy.CALL_LOOP, args);
  }

  /**
   * Sets the tail-recursiveness of the function.
   *
   * @param isTail whether or not the function is tail-recursive
   */
  @Override
  public void setTail(boolean isTail) {
    ((ClosureRootNode) callTarget.getRootNode()).setTail(isTail);
  }

  /**
   * Generates the provided function definition in the given stack {@code frame}.
   *
   * @param frame the stack frame for execution
   * @return the function defined by this node
   */
  @Override
  public Function executeFunction(VirtualFrame frame) {
    MaterializedFrame scope = frame.materialize();
    return new Function(callTarget, scope, this.schema);
  }

  /**
   * Executes the current node.
   *
   * @param frame the stack frame for execution
   * @return the result of executing the node
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return executeFunction(frame);
  }

  /**
   * Gets the call target associated with this function.
   *
   * @return the call target for this function
   */
  public RootCallTarget getCallTarget() {
    return callTarget;
  }

  /**
   * Gets the argument information from the function definition site.
   *
   * @return information on the function's arguments
   */
  public ArgumentDefinition[] getArgs() {
    return schema.getArgumentInfos();
  }
}
