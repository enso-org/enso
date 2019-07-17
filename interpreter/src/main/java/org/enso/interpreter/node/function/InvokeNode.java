package org.enso.interpreter.node.function;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.optimiser.TailCallException;
import org.enso.interpreter.runtime.*;
import org.enso.interpreter.runtime.errors.NotInvokableException;
import org.enso.interpreter.runtime.errors.TypeError;

@NodeInfo(shortName = "@", description = "Executes function")
@NodeChild("target")
public abstract class InvokeNode extends ExpressionNode {
  @Children private final ExpressionNode[] arguments;
  @Child private DispatchNode dispatchNode;

  public InvokeNode(ExpressionNode[] arguments) {
    this.arguments = arguments;
    this.dispatchNode = new SimpleDispatchNode();
  }

  @ExplodeLoop
  public Object[] computeArguments(VirtualFrame frame) {
    Object[] positionalArguments = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      positionalArguments[i] = arguments[i].executeGeneric(frame);
    }
    return positionalArguments;
  }

  @Specialization
  public Object invokeFunction(VirtualFrame frame, Function target) {
    Object[] positionalArguments = computeArguments(frame);

    CompilerAsserts.compilationConstant(this.isTail());
    if (this.isTail()) {
      throw new TailCallException(target, positionalArguments);
    } else {
      return dispatchNode.executeDispatch(target, positionalArguments);
    }
  }

  @Specialization
  public Atom invokeConstructor(VirtualFrame frame, AtomConstructor constructor) {
    Object[] positionalArguments = computeArguments(frame);
    return constructor.newInstance(positionalArguments);
  }

  @Fallback
  public Object invokeGeneric(VirtualFrame frame, Object target) {
    if (TypesGen.isFunction(target))
      return invokeFunction(frame, (Function) target);
    if (TypesGen.isAtomConstructor(target))
      return invokeConstructor(frame, (AtomConstructor) target);
    throw new NotInvokableException(target, this);
  }
}
