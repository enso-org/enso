package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNode;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNodeGen;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.MethodDoesNotExistException;
import org.enso.interpreter.runtime.error.NotInvokableException;
import org.enso.interpreter.runtime.type.TypesGen;

import java.util.Arrays;

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

  private final boolean canApplyThis;
  private final int thisArgumentPosition;

  @Child private ArgumentSorterNode argumentSorter;
  @Child private MethodResolverNode methodResolverNode;

  private final ConditionProfile methodCalledOnNonAtom = ConditionProfile.createCountingProfile();

  /**
   * Creates a new node for performing callable invocation.
   *
   * @param callArguments information on the arguments being passed to the {@link Function}
   */
  public InvokeCallableNode(CallArgument[] callArguments) {
    this.argExpressions =
        Arrays.stream(callArguments)
            .map(CallArgument::getExpression)
            .toArray(ExpressionNode[]::new);

    CallArgumentInfo[] argSchema =
        Arrays.stream(callArguments).map(CallArgumentInfo::new).toArray(CallArgumentInfo[]::new);

    boolean appliesThis = false;
    int idx = 0;
    for (; idx < argSchema.length; idx++) {
      CallArgumentInfo arg = argSchema[idx];
      if (arg.isPositional()
          || (arg.isNamed() && arg.getName().equals(Constants.THIS_ARGUMENT_NAME))) {
        appliesThis = true;
        break;
      }
    }
    this.canApplyThis = appliesThis;
    this.thisArgumentPosition = idx;

    this.argumentSorter = ArgumentSorterNodeGen.create(argSchema);
    this.methodResolverNode = MethodResolverNodeGen.create();
  }

  /**
   * Marks whether the {@code argumentSorter} child is tail–recursive.
   * @param isTail whether or not the node is tail-recursive.
   */
  @Override
  public void setTail(boolean isTail) {
    super.setTail(isTail);
    argumentSorter.setTail(isTail);
  }

  /**
   * Evaluates the arguments being passed to the function.
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
   * @param function the function to be executed
   * @return the result of executing {@code callable} on the known arguments
   */
  @Specialization
  public Object invokeFunction(VirtualFrame frame, Function function) {
    Object[] evaluatedArguments = evaluateArguments(frame);
    return this.argumentSorter.execute(function, evaluatedArguments);
  }

  /**
   * Invokes a constructor directly on the arguments contained in this node.
   *
   * @param frame the stack frame in which to execute
   * @param constructor the constructor to be executed
   * @return the result of executing {@code constructor} on the known arguments
   */
  @Specialization
  public Object invokeConstructor(VirtualFrame frame, AtomConstructor constructor) {
    return invokeFunction(frame, constructor.getConstructorFunction());
  }

  /**
   * Invokes a dynamic symbol after resolving it for the actual symbol for the {@code this}
   * argument.
   *
   * @param frame the stack frame in which to execute
   * @param symbol the name of the requested symbol
   * @return the result of resolving and executing the symbol for the {@code this} argument
   */
  @Specialization
  public Object invokeDynamicSymbol(VirtualFrame frame, UnresolvedSymbol symbol) {
    if (canApplyThis) {
      Object[] evaluatedArguments = evaluateArguments(frame);
      Object selfArgument = evaluatedArguments[thisArgumentPosition];
      if (methodCalledOnNonAtom.profile(TypesGen.isAtom(selfArgument))) {
        Atom self = (Atom) selfArgument;
        Function function = methodResolverNode.execute(symbol, self);
        return this.argumentSorter.execute(function, evaluatedArguments);
      } else {
        throw new MethodDoesNotExistException(selfArgument, symbol.getName(), this);
      }
    } else {
      throw new RuntimeException("Currying without `this` argument is not yet supported.");
    }
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
